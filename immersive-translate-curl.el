;;; immersive-translate-curl.el --- curl support -*- lexical-binding: t; -*-

;; Based on work by
;; Copyright (C) 2023  Karthik Chikmagalur
;; Original Author: Karthik Chikmagalur;; <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 2023/07/21
;; Most of the code in this file is taken from
;; https://github.com/karthink/gptel/blob/master/gptel-curl.el, with the
;; following modifications:
;; 1. Renamed function and variable names.
;; 2. Removed code related to streaming input.
;; 3. Modified part of the code to fit this project.


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'map)
(require 'json)
(require 'immersive-translate-chatgpt)
(require 'immersive-translate-baidu)

(defvar immersive-translate-service)
(declare-function immersive-translate-callback "ext:immersive-translate")

(defvar immersive-translate-curl--process-alist nil
  "Alist of active immersive-translate curl requests.")

(defcustom immersive-translate-curl-get-translation-alist
  '((chatgpt . immersive-translate-curl-chatgpt-get-translation)
	(baidu . immersive-translate-curl-baidu-get-translation))
  "Alist of functions to get the translated text.

Each element looks like (SERVICE . FUNCTION);

SERVICE is the translation service, see
`immersive-translate-service'; FUNCTION is a function of one
argument, called with RESPONSE (a JSON object) returned in
`immersive-translate-curl--parse-response'."
  :group 'immersive-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom immersive-translate-get-args-alist
  '((chatgpt . immersive-translate-chatgpt-get-args)
	(baidu . immersive-translate-baidu-get-args))
  "Alist of functions to produce list of arguments for calling Curl. .

Each element looks like (SERVICE . FUNCTION);

SERVICE is the translation service, see
`immersive-translate-service'; FUNCTION is a function of two
argument, called with CONTENT and TOKEN.

CONTENT is the data to send, TOKEN is a unique identifier."
  :group 'immersive-translate
  :type '(alist :key-type symbol :value-type function))

(defun immersive-translate-curl-chatgpt-get-translation (response)
  "Get the translated text return by CHATGPT."
  (map-nested-elt response '(:choices 0 :message :content)))

(defun immersive-translate-curl-baidu-get-translation (response)
  "Get the translated text return by BAIDU."
  (map-nested-elt response '(:trans_result 0 :dst)))

(defun immersive-translate-curl--parse-response (buf token)
  "Parse the buffer BUF with curl's response.

TOKEN is used to disambiguate multiple requests in a single
buffer."
  (with-current-buffer buf
    (progn
      (goto-char (point-max))
      (search-backward token)
      (backward-char)
      (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
        (goto-char (point-min))

        (if-let* ((http-msg (string-trim
                             (buffer-substring (line-beginning-position)
											   (line-end-position))))
                  (http-status
                   (save-match-data
                     (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                          (match-string 1 http-msg))))
                  (json-object-type 'plist)
                  (response (progn (goto-char header-size)
                                   (condition-case nil
                                       (json-read)
                                     (json-readtable-error 'json-read-error)))))
            (cond
             ((equal http-status "200")
              (list (string-trim
                     (or (funcall (alist-get
								   immersive-translate-service
								   immersive-translate-curl-get-translation-alist)
								  response)
						 ""))
                    http-msg))
             ((plist-get response :error)
              (let* ((error-plist (plist-get response :error))
                     (error-msg (plist-get error-plist :message))
                     (error-type (plist-get error-plist :type)))
                (list nil (concat "(" http-msg ") " (string-trim error-type)) error-msg)))
             ((eq response 'json-read-error)
              (list nil (concat "(" http-msg ") Malformed JSON in response.")
                    "Malformed JSON in response"))
             (t (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                      "Could not parse HTTP response.")))
          (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response."))))))

(defun immersive-translate-curl--sentinel (process _status)
  "Process sentinel for immersive-translate curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when-let* (((eq (process-status process) 'exit))
                (proc-info (alist-get process immersive-translate-curl--process-alist))
                (proc-token (plist-get proc-info :token))
				(proc-content (plist-get proc-info :content))
                (proc-callback (plist-get proc-info :callback)))
      (pcase-let ((`(,response ,http-msg ,error)
                   (immersive-translate-curl--parse-response proc-buf proc-token)))
        (plist-put proc-info :status http-msg)
        (when error (plist-put proc-info :error error))
		(when (and (plist-get proc-info :retry)
				   (string-empty-p response))
		  (setq response "No response."))
		(when (and proc-content
				   (string-empty-p response)
				   (not (plist-get proc-info :retry)))
		  (plist-put proc-info :retry t)
		  (immersive-translate-curl-get-response proc-info proc-callback))
        (funcall proc-callback response proc-info)))
    (setf (alist-get process immersive-translate-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun immersive-translate-url-get-args (content token)
  "Produce list of arguments for calling Curl.

CONTENT is the data to send, TOKEN is a unique identifier."
  (let ((fun (alist-get immersive-translate-service immersive-translate-get-args-alist)))
	(funcall fun content token)))

(defun immersive-translate-curl-get-response (info &optional callback)
  "Retrieve response to content in INFO.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (args (immersive-translate-url-get-args (plist-get info :content) token))
         (process (apply #'start-process "immersive-translate-curl"
                         (generate-new-buffer "*immersive-translate-curl*") "curl" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process immersive-translate-curl--process-alist)
            (nconc (list :token token
                         :callback (or callback
                                       #'immersive-translate-callback))
                   info))
      (set-process-sentinel process #'immersive-translate-curl--sentinel))))


(defun immersive-translate-curl-abort (buf)
  "Stop any active curl process associated with the current buffer."
  (interactive (list (current-buffer)))
  (if-let* ((proc-attrs
             (cl-find-if
              (lambda (proc-list)
				(eq (plist-get (cdr proc-list) :buffer) buf))
              immersive-translate-curl--process-alist))
            (proc (car proc-attrs)))
      (progn
        (setf (alist-get proc immersive-translate-curl--process-alist nil 'remove) nil)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (message "Stopped chatgpt request in buffer %S" (buffer-name buf)))
    (message "No chatgpt request associated with buffer %S" (buffer-name buf))))


(cl-defun immersive-translate-curl-do
    (&optional content &key callback
               (buffer (current-buffer))
               position)
  "Request a response from `immersive-translate-service' for CONTENT.

If PROMPT is
- a string, it is used as is
- A list of plists, it is used to create a full prompt suitable for
  sending to ChatGPT.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (a string) and INFO (a plist):

(callback RESPONSE INFO)

RESPONSE is nil if there was no response or an error.

The INFO plist has (at least) the following keys:
:content       - The content that was sent with the request
:buffer       - The buffer current when the request was sent.
:position     - marker at the point the request was sent.
:status       - Short string describing the result of the request

Example of a callback that messages the user with the response
and info:

(lambda (response info)
  (if response
      (let ((posn (marker-position (plist-get info :position)))
            (buf  (buffer-name (plist-get info :buffer))))
        (message \"Response for request from %S at %d: %s\"
                 buf posn response))
    (message \"request failed with message: %s\"
             (plist-get info :status))))

Or, for just the response:

(lambda (response _)
  ;; Do something with response
  (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

BUFFER is the buffer the request belongs to. If omitted the
current buffer is recorded.

POSITION is a buffer position (integer or marker). If omitted,
the value of (point) is recorded."
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
    (immersive-translate-curl-get-response info callback)))

(provide 'immersive-translate-curl)
;;; immersive-translate-curl.el ends here
