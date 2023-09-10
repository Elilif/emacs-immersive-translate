;;; immersive-translate-deepl.el --- DeepL translation backend -*- lexical-binding: t; -*-

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

(defgroup immersive-translate-deepl nil
  "Immersive translate deepl backend."
  :group 'immersive-translate)

(defcustom immersive-translate-deepl-target-language "ZH"
  "The target language.

See https://www.deepl.com/docs-api/translate-text/translate-text
for more defails."
  :group 'immersive-translate-deepl
  :type 'string)

(defcustom immersive-translate-deepl-api "https://api-free.deepl.com/v2/translate"
  "DeepL API.

See https://www.deepl.com/docs-api/translate-text/translate-text
for more defails."
  :group 'immersive-translate-deepl
  :type 'string)

(defun immersive-translate-deepl--request-data (content)
  "Encode data into JSON format..

Argument PROMPTS are for sending to DeepL."
  `(("text" . (,content))
    ("target_lang" . ,immersive-translate-deepl-target-language)))

(defun immersive-translate-deepl-get-args (content token)
  "Produce list of arguments for calling Curl.

CONTENT is the text to be translated, TOKEN is a unique
identifier."
  (let* ((url immersive-translate-deepl-api)
         (data (encode-coding-string
                (json-encode (immersive-translate-deepl--request-data content))
                'utf-8))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "DeepL-Auth-Key "
                                        (immersive-translate-api-key
                                         "deepl.com"
                                         "apikey"))))))
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

(defun immersive-translate-curl-deepl-get-translation (response)
  "Get the translated text in RESPONSE returned by DeepL."
  (or (map-nested-elt response '(:translations 0 :text))
      (let ((result (plist-get response :message)))
        (propertize result 'error t))))

(add-to-list 'immersive-translate-curl-get-translation-alist
             '(deepl . immersive-translate-curl-deepl-get-translation))

(add-to-list 'immersive-translate-curl-get-args-alist
             '(deepl . immersive-translate-deepl-get-args))

(defun immersive-translate-deepl-translate (info &optional callback)
  "Translate the content in INFO using Baidu Translation.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (immersive-translate-curl-do 'deepl info callback))

(provide 'immersive-translate-deepl)
;;; immersive-translate-deepl.el ends here
