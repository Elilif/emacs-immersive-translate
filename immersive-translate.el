;;; immersive-translate.el --- translate current buffer immersively -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/emacs-immersive-translate

;; Version: 0.1
;; Package-Requires: ((gptel))
;; Keywords: translation, gptel
;; SPDX-License-Identifier: GPL-3.0-or-later

;; TODO: use my own translation functions.
(require 'gptel)

(defgroup immersive-translate nil
  "Immersive translation")

(defcustom immersive-translate-gptel-system-prompt "You are a professional translator."
  "System prompt used by ChatGPT."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-gptel-user-prompt "You will be provided with text delimited by triple backticks, your task is to translate the wrapped text into Chinese. You should only output the translated text. \n```%s```"
  "User prompt used by ChatGPT."
  :group 'immersive-translate
  :type 'string)

(defun immersive-translate--elfeed-image-p ()
  "Return non-nil if current paragraph is an image."
  (string-match-p "^\\*$" (string-trim (thing-at-point 'paragraph t))))

(defun immersive-translate--info-code-block-p ()
  "Return non-nil if current paragraph is an code block."
  (string-match-p "^ \\{6,\\}" (thing-at-point 'paragraph t)))

(defcustom immersive-translate-disable-predicates '(immersive-translate--elfeed-image-p
													immersive-translate--info-code-block-p)
  "Predicates, return t when current paragraph should not to be translated.

Predicate functions don't take any arguments."
  :group 'immersive-translate
  :type '(repeat function))

(defun immersive-translate--info-get-paragraph ()
  (let* ((eop (save-excursion
				(end-of-paragraph-text)
				(point)))
		 (bop (save-excursion
				(or (re-search-forward " -- .*$" eop 'noerror)
					(progn
					  (start-of-paragraph-text)
					  (point)))))
		 (string (if bop
					 (buffer-substring-no-properties bop eop)
				   (buffer-substring-no-properties (point) eop))))
	(thread-last
	  string
	  (replace-regexp-in-string "\n" " ")
	  (replace-regexp-in-string " +" " "))))

(defun immersive-translate--get-paragraph ()
  (cond
   ((eq major-mode 'Info-mode)
	(immersive-translate--info-get-paragraph))
   (t
	(thing-at-point 'paragraph t))))

(defun immersive-translate--info-transform-response (str)
  (with-temp-buffer
	(insert str)
	(fill-region-as-paragraph (point-min) (point-max))
	(concat
	 "\n"
	 (replace-regexp-in-string "^" "     " (buffer-string))
	 "\n")))


(defun immersive-translate--transform-response (content-str)
  "Format CONTENT-STR."
  (cond
   ;; TODO: format info translations.
   ;; ((eq major-mode 'Info-mode)
   ;; 	(immersive-translate--info-transform-response content-str))
   (t
	(with-temp-buffer
	  (insert "\n")
	  (insert content-str)
	  (insert "\n")
	  (fill-region-as-paragraph (point-min) (point-max))
      (buffer-string)))))

(defun immersive-translate-callback (response info)
  "Insert RESPONSE from ChatGPT into current buffer.

INFO is a plist containing information relevant to this buffer.
See gptel--url-get-response for details."
  (let* ((status-str  (plist-get info :status))
         (origin-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position)))
	(with-current-buffer origin-buffer
	  (if response
		  (progn
			(setq response (immersive-translate--transform-response response))
			(save-excursion
			  (with-current-buffer (marker-buffer start-marker)
				(goto-char start-marker)
				(let ((ov (make-overlay (point) (1+ (point)))))
				  (overlay-put ov 'after-string response)))))
		(message "ChatGPT response error: (%s) %s"
				 status-str (plist-get info :error))))))

(defun immersive-translate-disable-p ()
  "Return non-nil if current paragraph should not to be translated.

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
	(while (and (re-search-forward "^\\s-*$")
				(not (eobp)))
	  (forward-line)
	  (immersive-translate-paragraph))))


;;;###autoload
(defun immersive-translate-paragraph ()
  "Translate current paragraph."
  (interactive)
  (save-excursion
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
  ;; FIXME: This function will delete all overlays.
  (remove-overlays))

(provide 'immersive-translate)
;;; immersive-translate.el ends here
