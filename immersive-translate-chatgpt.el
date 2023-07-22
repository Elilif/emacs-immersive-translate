;;; immersive-translate-chatgpt.el --- chatgpt backend for immersive-translation -*- lexical-binding: t; -*-

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
;; 3. Modified part of the code to fit this project.


;;; Code:

(require 'immersive-translate-curl)

(declare-function immersive-translate-api-key "ext:immersive-translate")

(defgroup immersive-translate-chatgpt nil
  "Immersive translate chatgpt backend."
  :group 'immersive-translate)

(defcustom immersive-translate-chatgpt-host "api.openai.com"
  "The OpenAI API host queried by immersive-translate."
  :group 'immersive-translate-chatgpt
  :type 'string)

(defcustom immersive-translate-chatgpt-model "gpt-3.5-turbo-0613"
  "GPT Model for chat.

The current options are
- \"gpt-3.5-turbo\"
- \"gpt-3.5-turbo-16k\"
- \"gpt-4\" (experimental)
- \"gpt-4-32k\" (experimental)"
  :group 'immersive-translate-chatgpt
  :type '(choice
          (const :tag "GPT 3.5 turbo" "gpt-3.5-turbo")
          (const :tag "GPT 3.5 turbo 16k" "gpt-3.5-turbo-16k")
          (const :tag "GPT 4 (experimental)" "gpt-4")
          (const :tag "GPT 4 32k (experimental)" "gpt-4-32k")))

(defcustom immersive-translate-chatgpt-temperature 0.7
  "\"Temperature\" of ChatGPT response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random."
  :group 'immersive-translate-chatgpt
  :type 'number)

(defcustom immersive-translate-chatgpt-proxy ""
  "Path to a proxy to use for ChatGPT query.
Passed to curl via --proxy arg, for example \"proxy.yourorg.com:80\"
Leave it empty if you don't use a proxy."
  :group 'immersive-translate-chatgpt
  :type 'string)

(defcustom immersive-translate-chatgpt-system-prompt "You are a professional translator."
  "System prompt used by ChatGPT."
  :group 'immersive-translate-chatgpt
  :type 'string)

(defcustom immersive-translate-chatgpt-user-prompt "You will be provided with text delimited by triple backticks, your task is to translate the wrapped text into Chinese. You should only output the translated text. \n```%s```"
  "User prompt used by ChatGPT."
  :group 'immersive-translate-chatgpt
  :type 'string)

(defun immersive-translate-chatgpt--request-data (prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,immersive-translate-chatgpt-model
                  :messages [,@prompts])))
    (plist-put prompts-plist :temperature immersive-translate-chatgpt-temperature)
    prompts-plist))

(defun immersive-translate-chatgpt-get-args (prompts token)
  "Produce list of arguments for calling Curl.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((url (format "https://%s/v1/chat/completions"
                      immersive-translate-chatgpt-host))
         (data (encode-coding-string
                (json-encode (immersive-translate-chatgpt--request-data prompts))
                'utf-8))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (immersive-translate-api-key
                                                   immersive-translate-chatgpt-host
                                                   "apikey"))))))
    (append
     (list "--location" "--silent" "--compressed" "--disable"
           (format "-X%s" "POST")
           (format "-w(%s . %%{size_header})" token)
           (format "-m%s" 60)
           "-D-"
           (format "-d%s" data))
     (when (not (string-empty-p immersive-translate-chatgpt-proxy))
       (list "--proxy" immersive-translate-chatgpt-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

(defun immersive-translate-chatgpt-create-prompt (content)
  "Create a full prompt suitable for sending to ChatGPT.

CONTENT is the text to be translated."
  (let ((user-prompt (format immersive-translate-chatgpt-user-prompt content)))
    `((:role "system" :content ,immersive-translate-chatgpt-system-prompt)
      (:role "user"   :content ,user-prompt))))

(defun immersive-translate-curl-chatgpt-get-translation (response)
  "Get the translated text return by CHATGPT."
  (map-nested-elt response '(:choices 0 :message :content)))

(add-to-list 'immersive-translate-curl-get-translation-alist
             '(chatgpt . immersive-translate-curl-chatgpt-get-translation))

(add-to-list 'immersive-translate-curl-get-args-alist
             '(chatgpt . immersive-translate-chatgpt-get-args))

(defun immersive-translate-chatgpt-translate (info &optional callback)
  (immersive-translate-curl-do 'chatgpt info callback))

(provide 'immersive-translate-chatgpt)
;;; immersive-translate-chatgpt.el ends here
