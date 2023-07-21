;;; immersive-translate-baidu.el --- baidu translation backend -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Eli Qian

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/emacs-immersive-translate

;; Version: v0.2.0
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'immersive-translate-curl)

(declare-function immersive-translate-api-key "ext:immersive-translate")
(declare-function immersive-translate-clear "ext:immersive-translate")

(defcustom immersive-translate-baidu-source-language "en"
  "The source language

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-baidu-target-language "zh"
  "The target language

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate
  :type 'string)

(defcustom immersive-translate-baidu-appid ""
  "The appid used by Baidu Trasnlate API.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate
  :type 'string)

(defvar immersive-translate-baidu-salt  (number-to-string (random 10000)))


(defun immersive-translate-baidu-generate-sign (content)
  "Generate the sign used by Baidu Trasnlate API.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  (md5 (concat
		immersive-translate-baidu-appid
		content
		immersive-translate-baidu-salt
		(immersive-translate-api-key
		 "fanyi-api.baidu.com"
		 immersive-translate-baidu-appid))))

(defun immersive-translate-baidu--request-data (content)
  "Generate data in POST request.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  (if (not (string-empty-p immersive-translate-baidu-appid))
	  (format "q=%s&from=%s&to=%s&appid=%s&salt=%s&sign=%s"
			  content
			  immersive-translate-baidu-source-language
			  immersive-translate-baidu-target-language
			  immersive-translate-baidu-appid
			  immersive-translate-baidu-salt
			  (immersive-translate-baidu-generate-sign content))
	(immersive-translate-clear)
	(user-error "Please set `immersive-translate-baidu-appid'!")))

(defun immersive-translate-baidu-get-args (content token)
  "Produce list of arguments for calling Curl.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((url "https://fanyi-api.baidu.com/api/trans/vip/translate")
         (data (encode-coding-string
                (immersive-translate-baidu--request-data content)
                'utf-8))
         (headers
          '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (append
     (list "--location" "--silent" "--compressed" "--disable"
           (format "-X%s" "POST")
           (format "-w(%s . %%{size_header})" token)
           (format "-m%s" 60)
           "-D-"
           (format "-d%s" data))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

(defun immersive-translate-baidu-translate (info &optional callback)
  (immersive-translate-curl-do 'baidu info callback))

(provide 'immersive-translate-baidu)
;;; immersive-translate-baidu.el ends here
