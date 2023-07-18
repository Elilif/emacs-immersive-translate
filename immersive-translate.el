;;; immersive-translate.el --- translate the current buffer immersively -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/emacs-immersive-translate

;; Version: 0.1
;; Package-Requires: ((gptel))
;; Keywords: translation, gptel
;; SPDX-License-Identifier: GPL-3.0-or-later

;; TODO: use my own translation functions.
(require 'gptel)

(defgroup immersive-translate nil
  "Immersive translation"
  :group 'applications)

(defcustom immersive-translate-gptel-system-prompt "You are a professional translator."
  "System prompt used by ChatGPT."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-gptel-user-prompt "You will be provided with text delimited by triple backticks, your task is to translate the wrapped text into Chinese. You should only output the translated text. \n```%s```"
  "User prompt used by ChatGPT."
  :group 'immersive-translate
  :type 'string)

(defun immersive-translate--elfeed-image-p ()
  "Return non-nil if the current paragraph is an image."
  (when (eq major-mode 'elfeed-show-mode)
	(string-match-p "^\\*$" (string-trim (thing-at-point 'paragraph t)))))

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
  (string-match-p "^\\*" (thing-at-point 'line t)))

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
	(end-of-paragraph-text)
	(when-let ((overlays (overlays-in (point) (1+ (point)))))
	  (cl-some (lambda (ov)
				 (overlay-get ov 'after-string))
			   overlays))))

(defcustom immersive-translate-disable-predicates '(immersive-translate--translation-exist-p
													immersive-translate--elfeed-image-p
													immersive-translate--info-code-block-p
													immersive-translate--info-menu-p
													immersive-translate--helpful-not-doc-p
													immersive-translate--help-signature-p)
  "Predicates, return t when the current paragraph should not to be translated.

Predicate functions don't take any arguments."
  :group 'immersive-translate
  :type '(repeat function))

(defvar-local immersive-translate--translation-overlays nil)

(defun immersive-translate--info-get-paragraph ()
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
  (let ((para (thing-at-point 'paragraph t)))
	(string-trim-left para "\nDocumentation\n")))

(defun immersive-translate--get-paragraph ()
  "Return the paragraph at point."
  (pcase major-mode
	('Info-mode
	 (immersive-translate--info-get-paragraph))
	('helpful-mode
	 (immersive-translate--helpful-get-paragraph))
	(_
	 (thing-at-point 'paragraph t))))

(defun immersive-translate--info-transform-response (str marker)
  "Format STR in `Info-mode'."
  (let* ((fill-column 70)
		 (str (string-trim-right str "[-=]+"))
		 (spaces (with-current-buffer (marker-buffer marker)
				   (save-excursion
					 (goto-char marker)
					 (re-search-backward "^\\( *\\)" (line-beginning-position))
					 (match-string 1)))))
	(with-temp-buffer
	  (insert str)
	  (fill-region-as-paragraph (point-min) (point-max))
	  (concat
	   "\n"
	   (replace-regexp-in-string "^" spaces (buffer-substring-no-properties
											 (point-min) (point-max)))
	   "\n"))))

(defun immersive-translate--get-fill-region-string (str)
  "Format STR."
  (with-temp-buffer
	(insert "\n")
	(insert str)
	(insert "\n")
	(fill-region-as-paragraph (point-min) (point-max))
	(buffer-string)))

(defun immersive-translate--nov-transform-response (str)
  "Format STR in `elfeed-show-mode.'"
  (let ((fill-column (or (and (boundp 'nov-text-width)
							  nov-text-width)
						 120)))
	(immersive-translate--get-fill-region-string str)))

(defun immersive-translate--elfeed-transform-response (str)
  "Format STR in `nov-mode'."
  (let ((fill-column (or (and (boundp 'shr-width)
							  shr-width)
						 110)))
	(immersive-translate--get-fill-region-string str)))

(defun immersive-translate--help-transform-response (str)
  "Format STR in `nov-mode'."
  (let ((fill-column 70))
	(immersive-translate--get-fill-region-string str)))


(defun immersive-translate--transform-response (content-str &optional marker)
  "Format CONTENT-STR."
  (pcase major-mode
	('Info-mode
	 (immersive-translate--info-transform-response content-str marker))
	('nov-mode
	 (immersive-translate--nov-transform-response content-str))
	('elfeed-show-mode
	 (immersive-translate--elfeed-transform-response content-str))
	((or 'helpful-mode
		 'help-mode)
	 (immersive-translate--help-transform-response content-str))
	(_
	 (immersive-translate--get-fill-region-string content-str))))

(defun immersive-translate-callback (response info)
  "Insert RESPONSE from ChatGPT into the current buffer.

INFO is a plist containing information relevant to this buffer.
See gptel--url-get-response for details."
  (let* ((status-str  (plist-get info :status))
         (origin-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position)))
	(with-current-buffer origin-buffer
	  (if response
		  (progn
			(setq response (immersive-translate--transform-response response start-marker))
			(save-excursion
			  (with-current-buffer (marker-buffer start-marker)
				(goto-char start-marker)
				(let ((ov (make-overlay (point) (1+ (point)))))
				  (overlay-put ov
							   'after-string
							   response)
				  (push ov immersive-translate--translation-overlays)))))
		(message "ChatGPT response error: (%s) %s"
				 status-str (plist-get info :error))))))

(defun immersive-translate-disable-p ()
  "Return non-nil if the current paragraph should not to be translated.

Nil otherwise."
  (cl-some (lambda (pred)
			 (funcall pred))
		   immersive-translate-disable-predicates))

;;;###autoload
(defun immersive-translate-buffer ()
  "Translate the whole buffer."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (and (re-search-forward "^\\s-*$" (point-max) 'noerror)
				(not (eobp)))
	  (forward-line)
	  (immersive-translate-paragraph))))


;;;###autoload
(defun immersive-translate-paragraph ()
  "Translate the current paragraph."
  (interactive)
  (save-excursion
	(forward-paragraph -1)
	(forward-line)
	(unless (immersive-translate-disable-p)
	  (when-let* ((content (immersive-translate--get-paragraph))
				  (gptel--system-message immersive-translate-gptel-system-prompt)
				  (user-prompt (format
								immersive-translate-gptel-user-prompt
								content)))
		(end-of-paragraph-text)
		(gptel-request user-prompt
					   :callback #'immersive-translate-callback)))))

;;;###autoload
(defun immersive-translate-clear ()
  "Clear translations."
  (interactive)
  (dolist (ov immersive-translate--translation-overlays)
	(delete-overlay ov))
  (setq immersive-translate--translation-overlays nil))

(provide 'immersive-translate)
;;; immersive-translate.el ends here
