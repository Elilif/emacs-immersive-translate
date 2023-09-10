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

(defvar immersive-translate--process-alist)
(defvar immersive-translate-failed-message)
(declare-function immersive-translate-callback "ext:immersive-translate")

(defgroup immersive-translate-curl nil
  "Immersive translate curl lib."
  :group 'immersive-translate)

(defcustom immersive-translate-curl-get-translation-alist '()
  "Alist of functions to get the translated text.

Each element looks like (SERVICE . FUNCTION);

SERVICE is the translation service, see
`immersive-translate-service'; FUNCTION is a function of one
argument, called with RESPONSE (a JSON object) returned in
`immersive-translate-curl--parse-response'."
  :group 'immersive-translate-curl
  :type '(alist :key-type symbol :value-type function))

(defcustom immersive-translate-curl-get-args-alist '()
  "Alist of functions to produce list of arguments for calling Curl. .

Each element looks like (SERVICE . FUNCTION);

SERVICE is the translation service; FUNCTION is a function of two
arguments, called with CONTENT and TOKEN.

CONTENT is the data to send, TOKEN is a unique identifier."
  :group 'immersive-translate-curl
  :type '(alist :key-type symbol :value-type function))

(defun immersive-translate-curl--parse-response (buf token service)
  "Parse the buffer BUF with curl's response.

TOKEN is used to disambiguate multiple requests in a single
buffer. SERVICE is the translation API to use."
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
                                   service
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
                (proc-info (alist-get process immersive-translate--process-alist))
                (proc-token (plist-get proc-info :token))
                (proc-content (plist-get proc-info :content))
                (proc-callback (plist-get proc-info :callback))
                (proc-service (plist-get proc-info :service)))
      (pcase-let ((`(,response ,http-msg ,error)
                   (immersive-translate-curl--parse-response proc-buf proc-token proc-service)))
        (plist-put proc-info :status http-msg)
        (when error (plist-put proc-info :error error))
        (when (and (plist-get proc-info :retry)
                   (or (string-empty-p response)
                       (get-text-property 0 'error response)))
          (setq response (concat
                          response
                          " "
                          immersive-translate-failed-message))
          (funcall proc-callback response proc-info))
        (when (and proc-content
                   (or (string-empty-p response)
                       (get-text-property 0 'error response))
                   (not (plist-get proc-info :retry)))
          (plist-put proc-info :retry t)
          (immersive-translate-curl-do proc-service proc-info proc-callback))
        (unless (plist-get proc-info :retry)
          (funcall proc-callback response proc-info))))
    (setf (alist-get process immersive-translate--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun immersive-translate-curl-do (service info &optional callback)
  "Retrieve response to content in INFO.

SERVICE is the translation API to use.Service.
INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (func (alist-get service immersive-translate-curl-get-args-alist))
         (args (funcall func (plist-get info :content) token))
         (process (apply #'start-process "immersive-translate-curl"
                         (generate-new-buffer "*immersive-translate-curl*") "curl" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process immersive-translate--process-alist)
            (nconc (list
                    :token token
                    :service service
                    :callback (or callback
                                  #'immersive-translate-callback))
                   info))
      (set-process-sentinel process #'immersive-translate-curl--sentinel))))

(provide 'immersive-translate-curl)
;;; immersive-translate-curl.el ends here
