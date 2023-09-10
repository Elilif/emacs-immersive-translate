;;; immersive-translate-baidu.el --- baidu translation backend -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Eli Qian

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/emacs-immersive-translate

;; Version: 0.3.0
;; SPDX-License-Identifier: GPL-3.0-or-later


;;; Commentary:
;; 

(require 'immersive-translate-curl)

;;; Code:

(declare-function immersive-translate-api-key "ext:immersive-translate")
(declare-function immersive-translate-clear "ext:immersive-translate")

(defgroup immersive-translate-baidu nil
  "Immersive translate baidu backend."
  :group 'immersive-translate)

(defcustom immersive-translate-baidu-source-language "en"
  "The source language.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate-baidu
  :type 'string)

(defcustom immersive-translate-baidu-target-language "zh"
  "The target language.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate-baidu
  :type 'string)

(defcustom immersive-translate-baidu-appid ""
  "The appid used by Baidu Trasnlate API.

See https://fanyi-api.baidu.com/doc/21 for more defails."
  :group 'immersive-translate-baidu
  :type 'string)

(defvar immersive-translate-baidu-salt  (number-to-string (random 10000)))


(defun immersive-translate-baidu-generate-sign (content)
  "Generate the sign used by Baidu Trasnlate API.

CONTENT is the text to be translated.
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

CONTENT is the text to be translated.
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

CONTENT is the text to be translated, TOKEN is a unique
identifier."
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

(defun immersive-translate-curl-baidu-get-translation (response)
  "Get the translated text in RESPONSE returned by BAIDU."
  (or (map-nested-elt response '(:trans_result 0 :dst))
      (let ((result (concat
                     (plist-get response :error_code)
                     ": "
                     (plist-get response :error_msg))))
        (propertize result 'error t))))

(add-to-list 'immersive-translate-curl-get-translation-alist
             '(baidu . immersive-translate-curl-baidu-get-translation))

(add-to-list 'immersive-translate-curl-get-args-alist
             '(baidu . immersive-translate-baidu-get-args))

(defun immersive-translate-baidu-translate (info &optional callback)
  "Translate the content in INFO using Baidu Translation.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (immersive-translate-curl-do 'baidu info callback))

(provide 'immersive-translate-baidu)
;;; immersive-translate-baidu.el ends here
