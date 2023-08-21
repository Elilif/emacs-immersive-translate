;;; immersive-translate.el --- Translate the current buffer immersively -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Eli Qian

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/emacs-immersive-translate
;; Keywords: convenience, help, translate
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Translate the current buffer immersively

;;; Code:

(require 'dom)
(require 'auth-source)
(require 'text-property-search)
(require 'immersive-translate-baidu)
(require 'immersive-translate-chatgpt)
(require 'immersive-translate-trans)



;;;; Customizations
(defgroup immersive-translate nil
  "Immersive translation."
  :group 'applications)

(defcustom immersive-translate-auto-idle 0.5
  "Perform translation the next time Emacs is idle for seconds."
  :group 'immersive-translate
  :type 'number)

(defcustom immersive-translate-backend 'baidu
  "The translation backend to use.

The current options are
- chatgpt
- baidu
- trans"
  :group 'immersive-translate
  :type '(choice
          (const :tag "ChatGPT" chatgpt)
          (const :tag "Baidu" baidu)
          (const :tag "translate-shell" trans)))

(defcustom immersive-translate-backend-alist
  '((baidu . immersive-translate-baidu-translate)
    (chatgpt . immersive-translate-chatgpt-translate)
    (trans . immersive-translate-trans-translate))
  "Alist of functions to do the translation job.

Each element looks like (BACKEND . FUNCTION);

BACKEND is the translation backend, see
`immersive-translate-backend'; FUNCTION is a function of two
argument."
  :group 'immersive-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom immersive-translate-pending-message "🔄"
  "Text displayed before the translation results are returned."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-failed-message "🔀"
  "Text displayed when translation fails."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-exclude-shr-tag '(html
                                                 base
                                                 body
                                                 a
                                                 audio
                                                 title
                                                 img
                                                 pre
                                                 script
                                                 style
                                                 svg
                                                 span
                                                 sub
                                                 sup
                                                 br
                                                 code
                                                 tt
                                                 hr
                                                 div
                                                 ol
                                                 ul
                                                 dl
                                                 strong
                                                 i
                                                 u
                                                 em
                                                 table)
  "HTML components that should not be translated."
  :group 'immersive-translate
  :type '(repeat symbol))

(defcustom immersive-translate-cache-directory (file-name-concat user-emacs-directory "var/immersive-translate/")
  "The directory where cached translations will be stored."
  :group 'immersive-translate
  :type 'directory)

(defun immersive-translate--info-code-block-p ()
  "Return non-nil if the current paragraph is a code block."
  (let ((current-line (thing-at-point 'line t)))
    (when (and
           (eq major-mode 'Info-mode)
           (string-match "^\\( \\{5,\\}\\)" current-line))
      (let* ((current-indent (length (match-string 1 current-line)))
             (prev-line (save-excursion
                          (forward-line -2)
                          (thing-at-point 'line t)))
             (prev-indent (if (string-match "^ +" prev-line)
                              (length (match-string 0 prev-line))
                            0))
             (prev-is-kb-desc? (string-prefix-p "‘" prev-line))
             (small-indent-change? (< (- current-indent prev-indent) 5)))
        (or (not (or prev-is-kb-desc? small-indent-change?))
            (string-match-p "^ +\(" current-line))))))

(defun immersive-translate--info-menu-p ()
  "Return non-nil if the current line is a menu."
  (when (eq major-mode 'Info-mode)
    (string-match-p "^\\*" (thing-at-point 'line t))))

(defun immersive-translate--helpful-not-doc-p ()
  "Return non-nil if point is not in the doc section."
  (when (eq major-mode 'helpful-mode)
    (when-let ((doc-beg (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^Documentation$" nil 'noerror)
                          (line-beginning-position)))
               (doc-end (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^References$" nil 'noerror)
                          (line-beginning-position))))
      (or (< (point) doc-beg)
          (>= (point) doc-end)))))

(defun immersive-translate--help-signature-p ()
  "Return non-nil if the current paragraph is a signature."
  (when (eq major-mode 'help-mode)
    (string-match-p "^ *\(" (thing-at-point 'line t))))

(defun immersive-translate--translation-exist-p ()
  "Return non-nil if the current paragraph has been translated."
  (save-excursion
    (immersive-translate-end-of-paragraph)
    (when-let ((overlays (overlays-in (1- (point)) (1+ (point)))))
      (cl-some (lambda (ov)
                 (overlay-get ov 'after-string))
               overlays))))

(defcustom immersive-translate-disable-predicates '(immersive-translate--translation-exist-p
                                                    immersive-translate--info-code-block-p
                                                    immersive-translate--info-menu-p
                                                    immersive-translate--helpful-not-doc-p
                                                    immersive-translate--help-signature-p)
  "Predicates, return t when the current paragraph should not be translated.

Predicate functions don't take any arguments."
  :group 'immersive-translate
  :type '(repeat function))

(defvar immersive-translate--process-alist nil
  "Alist of active immersive-translate curl requests.")
(defvar-local immersive-translate--translation-overlays nil)
(defvar immersive-translate--timer nil)
(defvar-local immersive-translate--window-start nil)
(defvar-local immersive-translate--window-end nil)

(defun immersive-translate-api-key (host user)
  "Lookup api key in the auth source.

By default, `immersive-translate-chatgpt-host' is used as HOST
and \"apikey\" as USER in Chatgpt backend.
\"fanyi-api.baidu.com\" is used as HOST and
`immersive-translate-baidu-appid' as USER in Baidu backend."
  (if-let ((secret (plist-get (car (auth-source-search
                                    :host host
                                    :user user
                                    :require '(:secret)))
                              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No %s found in the auth source" user)))


;;;; shr
(defun immersive-translate--shr-set-bound (orig dom)
  "Set DOM bound.

Advice for ORIG (shr-tag-*)."
  (let ((beg (point)))
    (funcall orig dom)
    (unless (= beg (point))
      (put-text-property beg (1+ beg) 'immersive-translate--beg (dom-tag dom))
      (save-excursion
        (save-restriction
          (widen)
          (skip-chars-backward "[ \n\t]")
          (put-text-property (1- (point)) (point) 'immersive-translate--end (dom-tag dom)))))))

(defun immersive-translate--shr-tag-advice (dom)
  "Add advice for DOM."
  (let* ((tag (dom-tag dom))
         (function
          (intern (concat "shr-tag-" (symbol-name tag)) obarray)))
    (unless (memq tag immersive-translate-exclude-shr-tag)
      (advice-add function :around #'immersive-translate--shr-set-bound))))


;;;; paragraph get functions
(defun immersive-translate--info-get-paragraph ()
  "Return the paragraph at point."
  (forward-paragraph -1)
  (forward-line)
  (let* ((eop (save-excursion
                (end-of-paragraph-text)
                (point)))
         (bop (save-excursion
                (re-search-forward "\\(^ [^ ].*\n \\{5,\\}\\)\\|\\(^ *‘.*’$\\)"
                                   (line-end-position 2)
                                   'noerror)))
         (string (if bop
                     (buffer-substring-no-properties bop eop)
                   (buffer-substring-no-properties (point) eop))))
    (thread-last
      string
      (replace-regexp-in-string "\n" " ")
      (replace-regexp-in-string " +" " "))))

(defun immersive-translate--helpful-get-paragraph ()
  "Return the paragraph at point."
  (forward-paragraph -1)
  (forward-line)
  (let ((para (thing-at-point 'paragraph t)))
    (string-trim-left para "\nDocumentation\n")))

(defun immersive-translate--elfeed-get-paragraph ()
  "Return the paragraph at point."
  (save-excursion
    (immersive-translate-end-of-paragraph)
    (when-let* ((beg-prop (text-property-search-backward 'immersive-translate--beg))
                (beg-pos (prop-match-beginning beg-prop))
                (tag (prop-match-value beg-prop))
                (end-prop (text-property-search-forward 'immersive-translate--end))
                (end-pos (prop-match-beginning end-prop)))
      (if (eq tag 'li)
          (buffer-substring-no-properties (+ beg-pos 2) end-pos)
        (buffer-substring-no-properties beg-pos end-pos)))))

(defun immersive-translate--elfeed-tube-get-paragraph ()
  "Return the paragraph at point."
  (forward-paragraph -1)
  (forward-line)
  (let ((para (thing-at-point 'paragraph t)))
    (string-trim-left para "\n\\[.*?\\] \\- \\[.*?\\]:\n")))

(defun immersive-translate--elfeed-tube-p (&optional mode)
  "Return non-nil if the current feed in MODE is a Youtube RSS feed.

Return nil otherwise."
  (when (eq mode 'elfeed-show-mode)
    (save-excursion
      (goto-char (point-min))
      (text-property-search-forward 'timestamp))))

(defun immersive-translate--get-paragraph ()
  "Return the paragraph at point."
  (pcase major-mode
    ('Info-mode
     (immersive-translate--info-get-paragraph))
    ('helpful-mode
     (immersive-translate--helpful-get-paragraph))
    ((pred immersive-translate--elfeed-tube-p)
     (immersive-translate--elfeed-tube-get-paragraph))
    ((or 'elfeed-show-mode 'nov-mode 'mu4e-view-mode)
     (immersive-translate--elfeed-get-paragraph))
    (_
     (thing-at-point 'paragraph t))))


;;;; format functions
(defun immersive-translate--get-indent (marker)
  "Return the indentation of the original text at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (save-excursion
        (re-search-backward "^\\( *\\)" (line-beginning-position))
        (match-string 1)))))

(defun immersive-translate--format-translation (str marker)
  "Function which produces the string to insert as a translation.

STR is the original translation. MARKER is the position where the
translation should be inserted."
  (with-temp-buffer
    (insert str)
    (fill-region-as-paragraph (point-min) (point-max))
    (concat
     "\n"
     (replace-regexp-in-string
      "^"
      (immersive-translate--get-indent marker)
      (buffer-substring-no-properties
       (point-min) (point-max)))
     "\n")))

(defun immersive-translate--info-transform-response (str marker)
  "Format STR at MARKER in `Info-mode'."
  (let* ((fill-column 70)
         (str (string-trim-right str "[-=*]+")))
    (immersive-translate--format-translation str marker)))

(defun immersive-translate--nov-transform-response (str marker)
  "Format STR at MARKER in `nov-mode'."
  (let ((fill-column (or (and (boundp 'nov-text-width)
                              nov-text-width)
                         120)))
    (immersive-translate--format-translation str marker)))

(defun immersive-translate--elfeed-transform-response (str marker)
  "Format STR at MARKER in `elfeed-mode'."
  (let ((fill-column (or (and (boundp 'shr-width)
                              shr-width)
                         110)))
    (save-excursion
      (goto-char marker)
      (let* ((beg-prop (text-property-search-backward 'immersive-translate--beg))
             (tag (prop-match-value beg-prop)))
        (concat (immersive-translate--format-translation str marker)
                (if (eq tag 'li)
                    "\n"
                  ""))))))

(defun immersive-translate--help-transform-response (str marker)
  "Format STR at MARKER in `help-mode'."
  (let ((fill-column 70))
    (immersive-translate--format-translation str marker)))

(defun immersive-translate--transform-response (content-str &optional marker)
  "Format CONTENT-STR at MARKER."
  (pcase major-mode
    ('Info-mode
     (immersive-translate--info-transform-response content-str marker))
    ('nov-mode
     (immersive-translate--nov-transform-response content-str marker))
    ((or 'elfeed-show-mode 'mu4e-view-mode)
     (immersive-translate--elfeed-transform-response content-str marker))
    ((or 'helpful-mode
         'help-mode)
     (immersive-translate--help-transform-response content-str marker))
    (_
     (immersive-translate--format-translation content-str marker))))


;;;; utility functions
(defun immersive-translate--cache-filename (content)
  "Generate the full filename based on CONTENT."
  (let ((hash (sha1 (concat
                     (prin1-to-string content)
                     (symbol-name immersive-translate-backend)))))
    (expand-file-name hash immersive-translate-cache-directory)))

(defun immersive-translate--cache-translation (new-content origin-content)
  "Save NEW-CONTENT to a file.

The filename is based on ORIGIN-CONTENT."
  (let ((file (immersive-translate--cache-filename origin-content)))
    (unless (file-directory-p immersive-translate-cache-directory)
      (make-directory immersive-translate-cache-directory t))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert new-content)))))

(defun immersive-translate--cache-p (content)
  "Return t if CONTENT is cached, else nil."
  (let* ((file (immersive-translate--cache-filename content)))
    (file-exists-p file)))

(defun immersive-translate--cache-get (content)
  "Get the cache for CONTENT."
  (let* ((file (immersive-translate--cache-filename content)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun immersive-translate--add-ov (response)
  "Add an after-string overlay after point.

The value of after-string is RESPONSE."
  (let ((ovs (overlays-in (1- (point)) (point)))
        (new-ov (make-overlay (point) (1+ (point)))))
    (mapc (lambda (ov)
            (when (overlay-get ov 'after-string)
              (delete-overlay ov)))
          ovs)
    (overlay-put new-ov
                 'after-string
                 response)
    (push new-ov immersive-translate--translation-overlays)))

(defun immersive-translate-callback (response info)
  "Insert RESPONSE from ChatGPT into the current buffer.

INFO is a plist containing information relevant to this buffer."
  (when-let* (((not (string-empty-p response)))
              (status-str  (plist-get info :status))
              (origin-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (origin-content (plist-get info :content)))
    (with-current-buffer origin-buffer
      (if response
          (progn
            (setq response (immersive-translate--transform-response response start-marker))
            (save-excursion
              (with-current-buffer (marker-buffer start-marker)
                (goto-char start-marker)
                (immersive-translate--add-ov response)
                (unless (string= response immersive-translate-failed-message)
                  (immersive-translate--cache-translation response origin-content)))))
        (message "Response error: (%s) %s"
                 status-str (plist-get info :error))))))

(defun immersive-translate-disable-p ()
  "Return non-nil if the current paragraph should not be translated.

Nil otherwise."
  (cl-some (lambda (pred)
             (funcall pred))
           immersive-translate-disable-predicates))

(defun immersive-translate-end-of-paragraph ()
  "Move to the end of the current paragraph."
  (pcase major-mode
    ((and (or 'elfeed-show-mode 'nov-mode 'mu4e-view-mode)
          (pred (not immersive-translate--elfeed-tube-p)))
     (unless (get-text-property (point) 'immersive-translate--beg)
       (text-property-search-backward 'immersive-translate--beg))
     (text-property-search-forward 'immersive-translate--end)
     (end-of-line))
    (_ (end-of-paragraph-text))))

(defun immersive-translate-join-lin (paragraph)
  "Form the current PARAGRAPH into a continuous paragraph."
  (when paragraph
    (let ((new-str (string-clean-whitespace
                    (replace-regexp-in-string "\n" " " paragraph))))
      (unless (string-empty-p new-str)
        new-str))))

(defun immersive-translate-region (start end)
  "Translate the text between START and END."
  (save-excursion
    (goto-char start)
    (pcase major-mode
      ((and (or 'elfeed-show-mode 'nov-mode 'mu4e-view-mode)
            (pred (not immersive-translate--elfeed-tube-p)))
       (while (and (text-property-search-forward 'immersive-translate--end)
                   (< (point) end))
         (immersive-translate-paragraph)))
      (_ (while (and
                 (< (point) end)
                 (re-search-forward "^\\s-*$" end 'noerror)
                 (not (eobp)))
           (forward-line)
           (immersive-translate-paragraph))))))

;;;###autoload
(defun immersive-translate-abort (buf)
  "Stop all active immersive-translate processes in BUF."
  (interactive (list (current-buffer)))
  (if-let* ((proc-attrs
             (cl-remove-if-not
              (lambda (proc-list)
                (eq (plist-get (cdr proc-list) :buffer) buf))
              immersive-translate--process-alist)))
      (dolist (proc-attr proc-attrs)
        (let ((proc (car proc-attr)))
          (setf (alist-get proc immersive-translate--process-alist nil 'remove) nil)
          (set-process-sentinel proc #'ignore)
          (delete-process proc)
          (kill-buffer (process-buffer proc))
          (message "Stopped all immersive-translate processes in buffer %S" (buffer-name buf))))
    (message "No immersive-translate process associated with buffer %S" (buffer-name buf))))

(cl-defun immersive-translate-do-translate
    (&optional content &key callback
               (buffer (current-buffer))
               position
               (backend immersive-translate-backend))
  "Request a response from `immersive-translate-backend' for CONTENT.

If PROMPT is
- a string, it is used as is
- A list of plists, it is used to create a full prompt suitable for
  sending to ChatGPT.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (a string) and INFO (a plist):

\(callback RESPONSE INFO)

RESPONSE is nil if there was no response or an error.

The INFO plist has (at least) the following keys:
:content       - The content that was sent with the request
:buffer       - The buffer current when the request was sent.
:position     - marker at the point the request was sent.
:status       - Short string describing the result of the request

Example of a callback that messages the user with the response
and info:

\(lambda (response info)
  (if response
      (let ((posn (marker-position (plist-get info :position)))
            (buf  (buffer-name (plist-get info :buffer))))
        (message \"Response for request from %S at %d: %s\"
                 buf posn response))
    (message \"request failed with message: %s\"
             (plist-get info :status))))

Or, for just the response:

\(lambda (response _)
  ;; Do something with response
  (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

BUFFER is the buffer the request belongs to. If omitted the
current buffer is recorded.

POSITION is a buffer position (integer or marker). If omitted,
the value of (point) is recorded.

BACKEND is the translation backend to use. If omitted, the value
of `immersive-translate-backend' is used."
  (let* ((start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
              (point-marker)))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
         (info (list :content content
                     :buffer buffer
                     :position start-marker)))
    (funcall (alist-get backend immersive-translate-backend-alist) info callback)))

;;;###autoload
(defun immersive-translate-buffer ()
  "Translate the whole buffer."
  (interactive)
  (pcase major-mode
    ((pred immersive-translate--elfeed-tube-p)
     (save-excursion
       (goto-char (point-min))
       (let ((beg (search-forward "Transcript:\n" (point-max) 'noerror)))
         (immersive-translate-region beg (point-max)))))
    (_
     (immersive-translate-region (point-min) (point-max)))))

;;;###autoload
(defun immersive-translate-paragraph ()
  "Translate the current paragraph."
  (interactive)
  (save-excursion
    (unless (immersive-translate-disable-p)
      (when-let* ((paragraph (immersive-translate-join-lin
                              (immersive-translate--get-paragraph)))
                  (content (if (eq immersive-translate-backend 'chatgpt)
                               (immersive-translate-chatgpt-create-prompt paragraph)
                             paragraph))
                  (ov t))
        (immersive-translate-end-of-paragraph)
        (if (immersive-translate--cache-p content)
            (let ((translation (immersive-translate--cache-get content)))
              (immersive-translate--add-ov translation))
          (setq ov (make-overlay (1- (point)) (point)))
          (overlay-put ov
                       'after-string
                       immersive-translate-pending-message)
          (immersive-translate-do-translate content))))))

;;;###autoload
(defun immersive-translate-clear ()
  "Clear translations."
  (interactive)
  (let ((ovs (overlays-in (point-min) (point-max))))
    (mapc (lambda (ov)
            (when (overlay-get ov 'after-string)
              (delete-overlay ov)))
          ovs))
  (dolist (ov immersive-translate--translation-overlays)
    (delete-overlay ov))
  (setq immersive-translate--translation-overlays nil))

(defun immersive-translate--auto-translate-window ()
  "Translate the text in the current window."
  (let ((start (window-start))
        (end (window-end)))
    (setq immersive-translate--window-start start
          immersive-translate--window-end end)
    (immersive-translate-region start end)))

;;;###autoload
(define-minor-mode immersive-translate-auto-mode
  "Toggle `immersive-translate-auto-mode'.

Translate paragraph under the cursor after Emacs is idle for
`immersive-translate-auto-idle' seconds."
  :global nil
  :group 'immersive-translate
  (cond
   (immersive-translate-auto-mode
    (when (and immersive-translate--timer (timerp immersive-translate--timer))
      (cancel-timer immersive-translate--timer))
    (immersive-translate--auto-translate-window)
    (setq immersive-translate--timer
          (run-with-idle-timer immersive-translate-auto-idle t #'immersive-translate--auto-translate)))
   (t
    (cancel-timer immersive-translate--timer)
    (setq immersive-translate--timer nil
          immersive-translate--window-start nil
          immersive-translate--window-end nil))))

(defun immersive-translate--auto-translate ()
  "Helper Function for `immersive-translate-auto-mode'."
  (when (and immersive-translate-auto-mode
             immersive-translate--window-end
             (or (< (window-start) immersive-translate--window-start)
                 (> (window-end) immersive-translate--window-end)))
    (immersive-translate--auto-translate-window)))

;;;###autoload
(defun immersive-translate-setup ()
  "Setup immersive-trasnlate."
  (advice-add 'shr-descend :before #'immersive-translate--shr-tag-advice))


;;;; provide
(provide 'immersive-translate)
;;; immersive-translate.el ends here
