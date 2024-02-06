;;; gpt-commit.el --- Commit messages with GPT -*- lexical-binding: t; -*-

;; Author: Youngwook Kim <youngwook.kim@gmail.com>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gpt-commit
;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "29.1") (magit "3.3.0") (transient "0.5.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: convenience
;; Version: 0.1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GPT-Commit is an Emacs package that automates the generation of
;; conventional commit messages.  By leveraging the power of GPT
;; (Generative Pre-trained Transformer) models, it suggests structured
;; commit messages following the conventional commit format.
;;
;; Features:
;; - Automatic generation and refining of conventional commit messages
;; - Transient menu and integration with Git and Magit for seamless workflow
;; - Easy configuration and customization
;;
;;
;; (require 'gpt-commit)
;; (require 'git-commit)
;; (setq gpt-commit-api-key "YOUR_OPENAI_API_KEY")
;; (setq gpt-commit-model "gpt-4")
;; (define-key git-commit-mode-map (kbd "C-c *") #'gpt-commit-message)

;;; Code:


(require 'transient)
(require 'subr-x)


(defcustom gpt-commit-types-alist '(("feat"
                                     (description . "A new feature")
                                     (title . "Features")
                                     (emoji . "✨"))
                                    ("fix"
                                     (description . "A bug fix")
                                     (title . "Bug Fixes")
                                     (emoji . "🐛"))
                                    ("docs"
                                     (description . "Documentation only changes")
                                     (title . "Documentation")
                                     (emoji . "📚"))
                                    ("style"
                                     (description . "Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)")
                                     (title . "Styles")
                                     (emoji . "💎"))
                                    ("refactor"
                                     (description . "A code change that neither fixes a bug nor adds a feature")
                                     (title . "Code Refactoring")
                                     (emoji . "📦"))
                                    ("perf"
                                     (description . "A code change that improves performance")
                                     (title . "Performance Improvements")
                                     (emoji . "🚀"))
                                    ("test"
                                     (description . "Adding missing tests or correcting existing tests")
                                     (title . "Tests")
                                     (emoji . "🚨"))
                                    ("build"
                                     (description . "Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)")
                                     (title . "Builds")
                                     (emoji . "🏗️"))
                                    ("ci"
                                     (description . "Changes to our CI configuration files and scripts (example scopes: Travis, Circle, BrowserStack, SauceLabs)")
                                     (title . "Continuous Integrations")
                                     (emoji . "⚙️"))
                                    ("chore"
                                     (description . "Other changes that don't modify src or test files")
                                     (title . "Chores")
                                     (emoji . "♻️"))
                                    ("revert"
                                     (description . "Reverts a previous commit")
                                     (title . "Reverts")
                                     (emoji . "↩️")))
  "A key-value-alist of commit types."
  :group 'gpt-commit
  :type '(alist
          :key-type string
          :value-type (set :tag "Annotation rules"
                           (cons (const :tag "" title)
                                 (string))
                           (cons :tag "Description"
                                 (const :tag "" description)
                                 (string))
                           (cons :tag "Emoji"
                                 (const :tag "" emoji)
                                 (string)))))

(defcustom gpt-commit-annotation-spec-alist '((emoji " %s " 15)
                                              (description "%s" 80))
  "Alist of symbol, format string and width for displaying commit type."
  :group 'gpt-commit
  :type '(alist
          :key-type symbol
          :value-type (list
                       (string :tag "Column Name" "%s")
                       (integer :tag "Column Width" 20))))

(defcustom gpt-commit-use-stream t
  "Flag to use streaming for GPT commit message generation.

Determines whether to use streaming for GPT commit message generation.

When non-nil, streaming is enabled, allowing for incremental output as the GPT
model generates the commit message. This can be useful for large requests or
when immediate feedback is desired.

To abort a streaming request, use the command `gpt-commit-stream-abort'.

The default value is t, meaning streaming is enabled by default.

When set to nil, streaming is disabled, and the commit message will only be
displayed once fully generated.

This setting is applicable to functions like `gpt-commit-improve-message' and
`gpt-commit-message' that rely on GPT model responses.

Toggle this setting to switch between streaming and non-streaming modes as
needed."
  :group 'gpt-commit
  :type 'boolean)


(defcustom gpt-commit-system-prompt-en "Based on the user-supplied incomplete or empty commit message and the output from `git diff --cached`, your task is to generate a completed, conventional commit message accurately encompassing the changes. Ensure that your response strictly contains the refined commit message, without including any extraneous information. Fill text lines to be no longer than 70, don't wrap in any quotes."
  "Prompt (directive) for GPT-commit to generate commit message."
  :type 'string
  :group 'gpt-commit)

(defcustom gpt-commit-improve-system-prompt "The user will feed you a commit message. Your job is to evaluate the grammar and refine it if necessary. Make sure to preserve the original meaning and maintain the context of the message. The refined message should remain the only content in your response, and ensure that the lines do not surpass 70 characters in length."
  "Prompt (directive) for GPT-commit to improve commit message."
  :type 'string
  :group 'gpt-commit)


(defcustom gpt-commit-system-prompts '("Based on the user-supplied incomplete or empty commit message and the output from `git diff --cached`, your task is to generate a completed, conventional commit message accurately encompassing the changes. Ensure that your response strictly contains the refined commit message, without including any extraneous information. Fill text lines to be no longer than 70, don't wrap in any quotes."
                                       "Using the output from `git diff --cached` and any user-provided partial commit message, your task is to create a concise commit summary starting with a scope (e.g., feat, fix, docs, style, refactor, perf, test, build, ci, chore or revert). The summary should not exceed 70 characters and must encapsulate the primary change or intent of the commit succinctly. Don't wrap summary in any quotes. Provide only the summary, excluding any additional information or detailed description. ")
  "List of strings for GPT-based commit message generation.

A list of system prompts used to guide the generation of commit messages with
the help of a GPT model. Each prompt is a string that instructs the GPT model on
how to process the `git diff --cached` output and any partial commit message
provided by the user to create a complete and conventional commit message.

Each prompt should be crafted to provide clear and concise instructions to the
GPT model, ensuring that the generated commit message is relevant and adheres to
conventional commit standards. The prompts should not include any extraneous
information and should be formatted to fit within a 70-character width limit.

To use these prompts, select one from the list as the active prompt when
invoking the commit message generation function. The selected prompt will be
sent to the GPT model along with the `git diff --cached` output and any
user-provided commit message fragment to generate a complete commit message."
  :type '(repeat string)
  :group 'gpt-commit)


(defcustom gpt-commit-api-url "https://api.openai.com/v1/chat/completions"
  "The URL for the OpenAI API endpoint to use for GPT chat completions."
  :group 'gpt-commit
  :type 'string)

(defcustom gpt-commit-model "gpt-4-1106-preview"
  "The model to use for GPT requests.

The effectiveness of commit message generation depends on the
chosen model. Different models may produce different results."
  :group 'gpt-commit
  :type '(radio
          (const "gpt-4-1106-preview")
          (const "gpt-4")
          (const "gpt-4-0314")
          (const "gpt-4-0613")
          (const "gpt-3.5-turbo-1106")
          (const "gpt-3.5-turbo-0613")
          (const "gpt-3.5-turbo-16k-0613")
          (const "gpt-3.5-turbo-16k")
          (const "gpt-3.5-turbo-0301")
          (const "gpt-3.5-turbo")
          (string "other")))

(defvar gpt-commit-curr-prompt-idx 0)

(defcustom gpt-commit-api-key ""
  "API key for GPT service as a string or a function that returns the API key.
If it is a function, it will be called without arguments and should return the
API key for gpt service when called."
  :group 'gpt-commit
  :type '(radio
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom gpt-commit-gpt-temperature 0.1
  "The randomness of the GPT model's output.

A higher value increases the randomness, while a lower value makes the
output more deterministic.

It should be a floating-point number between 0.0 and 2.0."
  :group 'gpt-commit
  :type 'number)

(defcustom gpt-commit-post-response-hook nil
  "Functions to run after the whole stream response finished and inserted.

The functions attached to this hook will be called with no arguments. They
should perform operations based on the state of the program at the time
they are called.

Note that the order in which functions are added to the hook determines the
order in which they are called. The most recently added function is called
first."
  :group 'gpt-commit
  :type 'hook)

(defcustom gpt-commit-pre-response-hook nil
  "Functions to run before the inserting response from the GPT request.

This hook runs without any arguments.

Note that the order in which functions are added to the hook determines the
order in which they are called. The most recently added function is called
first."
  :group 'gpt-commit
  :type 'hook)

(declare-function json-read "json")
(declare-function json-encode "json")
(declare-function json-read-from-string "json")

(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)

(defun gpt-commit-json-read-buffer (&optional object-type array-type null-object
                                    false-object)
  "Parse json from the current buffer using specified object and array types.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))



(defun gpt-commit--api-key ()
  "Retrieve the OpenAI API key, either directly or from a function."
  (if (functionp
       'gpt-commit-api-key)
      (funcall
       gpt-commit-api-key)
    gpt-commit-api-key))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-false)
(defvar json-null)
(defvar url-request-method)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-http-end-of-headers)

(declare-function json-encode "json")
(declare-function json-read "json")
(declare-function json-read-from-string "json")
(declare-function url-host "url-parse")
(declare-function auth-source-search "auth-source")

(defcustom gpt-commit-abort-on-keyboard-quit-count 3
  "Number of `keyboard-quit' presses before aborting GPT documentation requests.

Determines the number of consecutive `keyboard-quit' commands needed to abort an
active streaming request.

The default value is 3, meaning that pressing `keyboard-quit' three times in
quick succession will abort the request.

This variable is only effective when `gpt-commit-use-stream' is non-nil, as
it applies to streaming requests.

If the number of `keyboard-quit' commands does not reach the set threshold, the
abort action will not be triggered."
  :group 'gpt-commit
  :type 'integer)

(defcustom gpt-commit-debug nil
  "Whether to enable debugging in the GPT documentation group."
  :group 'gpt-commit
  :type 'boolean)


(defcustom gpt-commit-api-key 'gpt-commit-api-key-from-auth-source
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'gpt-commit
  :type '(radio
          (string :tag "API key")
          (function-item gpt-commit-api-key-from-auth-source)
          (function :tag "Function that returns the API key")))

(defcustom gpt-commit-gpt-url "https://api.openai.com/v1/chat/completions"
  "The URL to the OpenAI GPT API endpoint for chat completions."
  :group 'gpt-commit
  :type 'string)

(defcustom gpt-commit-gpt-temperature 0.1
  "The temperature for the OpenAI GPT model used.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random.

The temperature controls the randomness of the output generated by the model. A
lower temperature results in more deterministic and less random completions,
while a higher temperature produces more diverse and random completions.

To adjust the temperature, set the value to the desired level. For example, to
make the model's output more deterministic, reduce the value closer to 0.1.
Conversely, to increase randomness, raise the value closer to 1.0."
  :group 'gpt-commit
  :type 'number)


(defvar gpt-commit--request-url-buffers nil
  "Alist of active request buffers requests.")

(defvar gpt-commit--debug-data-raw nil
  "Stores raw data for debugging purposes.")

(defvar auth-sources)

(defun gpt-commit-api-key-from-auth-source (&optional url)
  "Return the fist API key from the auth source for URL.
By default, the value of `gpt-commit-gpt-url' is used as URL."
  (require 'auth-source)
  (require 'url-parse)
  (let* ((host
          (url-host (url-generic-parse-url (or url gpt-commit-gpt-url))))
         (secret (plist-get (car (auth-source-search
                                  :host host))
                            :secret)))
    (pcase secret
      ((pred not)
       (user-error (format "No `gpt-commit-api-key' found in the auth source.
Your auth-sources %s should contain such entry:
machine %s password TOKEN" auth-sources host)))
      ((pred functionp)
       (encode-coding-string (funcall secret) 'utf-8))
      (_ secret))))

(defun gpt-commit-get-api-key ()
  "Return the value of `gpt-commit-api-key' if it is a function.
If it is a string, prompt the user for a key, save it, and renturn the key.
If `gpt-commit-api-key' is not set, raise an error."
  (pcase gpt-commit-api-key
    ((pred functionp)
     (funcall gpt-commit-api-key))
    ((pred stringp)
     (while (string-empty-p gpt-commit-api-key)
       (let ((key (read-string "GPT Api Key: ")))
         (customize-set-value 'gpt-commit-api-key key)
         (when (yes-or-no-p "Save this key?")
           (customize-save-variable 'gpt-commit-api-key key))))
     gpt-commit-api-key)
    (_ (error "`gpt-commit-api-key' is not set"))))


(defun gpt-commit--json-parse-string (str &optional object-type array-type
                                null-object false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))


(defun gpt-commit--abort-all ()
  "Cancel all pending GPT document requests."
  (gpt-commit--abort-by-url-buffer t))

(defun gpt-commit-abort-all ()
  "Terminate the process associated with a buffer BUFF and delete its buffer.

Argument BUFF is the buffer in which the process to be aborted is running."
  (interactive)
  (gpt-commit--abort-all))

(defun gpt-commit-restore-text-props ()
  "Restore old text properties after removing `gpt-commit' props."
  (pcase-let* ((`(,beg . ,end)
                (gpt-commit--property-boundaries 'gpt-commit-old))
               (value (and beg
                           (get-text-property beg 'gpt-commit-old))))
    (when (and beg end)
      (remove-text-properties beg end '(gpt-commit t gpt-old-doc t)))
    (when beg
      (goto-char beg)
      (delete-region beg end)
      (when (stringp value)
        (insert value)))))

(defun gpt-commit-goto-char (position)
  "Jump to POSITION in all windows displaying the buffer.

Argument POSITION is the buffer position to go to."
  (goto-char position)
  (dolist (wnd (get-buffer-window-list (current-buffer) nil t))
    (set-window-point wnd position)))

(defun gpt-commit-abort-buffer (buffer)
  "Cancel ongoing URL request for buffer.

Argument BUFFER is the buffer associated with a request to be aborted."
  (pcase-dolist (`(,req-buff . ,marker) gpt-commit--request-url-buffers)
    (let ((buff
           (when (markerp marker)
             (marker-buffer marker))))
      (when (eq buffer buff)
        (gpt-commit--abort-by-url-buffer req-buff)))))

(defun gpt-commit--property-boundaries (prop &optional pos)
  "Return boundaries of property PROP at POS (cdr is 1+)."
  (unless pos (setq pos (point)))
  (let (beg end val)
    (setq val (get-text-property pos prop))
    (if (null val)
        val
      (if (or (bobp)
              (not (eq (get-text-property (1- pos) prop) val)))
          (setq beg pos)
        (setq beg (previous-single-property-change pos prop))
        (when (null beg)
          (setq beg (point-min))))
      (if (or (eobp)
              (not (eq (get-text-property (1+ pos) prop) val)))
          (setq end pos)
        (setq end (next-single-property-change pos prop))
        (when (null end)
          (setq end (point-min))))
      (cons beg end))))

(defun gpt-commit-remove-text-props ()
  "Strip `gpt-commit' text properties from a document region."
  (pcase-let ((`(,beg . ,end)
               (gpt-commit--property-boundaries 'gpt-commit)))
    (when (and beg end)
      (remove-text-properties beg end '(gpt-commit t gpt-old-doc t)))))


(defun gpt-commit-abort-current-buffer ()
  "Cancel processing in the active buffer."
  (let ((buff (current-buffer)))
    (gpt-commit-abort-buffer buff)))

(defun gpt-commit--abort-by-url-buffer (url-buff)
  "Cancel ongoing URL fetch and close buffer.

Argument URL-BUFF is the buffer associated with the URL retrieval process to be
aborted."
  (pcase-dolist (`(,req-buff . ,marker) gpt-commit--request-url-buffers)
    (when (or (eq url-buff t)
              (eq req-buff url-buff))
      (when (buffer-live-p req-buff)
        (let ((proc (get-buffer-process req-buff)))
          (when proc
            (delete-process proc))
          (kill-buffer req-buff))))
    (gpt-commit--abort-by-marker marker))
  (setq gpt-commit--request-url-buffers
        (if (eq url-buff t)
            nil
          (assq-delete-all url-buff gpt-commit--request-url-buffers)))
  (when (symbol-value 'gpt-commit-abort-mode)
    (gpt-commit-abort-mode -1)))

(defun gpt-commit--retrieve-error (status)
  "Extract and format error details from STATUS.

Argument STATUS is a plist containing the status of the HTTP request."
  (pcase-let*
      ((status-error (plist-get status :error))
       (`(_err ,type ,code) status-error)
       (description
        (and status-error
             (progn
               (when (and (boundp
                           'url-http-end-of-headers)
                          url-http-end-of-headers)
                 (goto-char url-http-end-of-headers))
               (when-let ((err (ignore-errors
                                 (cdr-safe
                                  (assq 'error
                                        (gpt-commit-json-read-buffer
                                         'alist))))))
                 (or (cdr-safe (assq 'message err)) err))))))
    (when status-error
      (let* ((prefix (if (facep 'error)
                         (propertize
                          "gpt-commit error"
                          'face
                          'error)
                       "gpt-commit error"))
             (details (delq nil
                            (list
                             (when type  (format "%s request failed" type))
                             (when code (format "with status %s" code))
                             (when description (format "- %s" description))))))
        (if details
            (concat prefix ": "
                    (string-join
                     details
                     " "))
          prefix)))))

(defun gpt-commit--get-response-content (response)
  "Retrieve and decode content from a RESPONSE object.

Argument RESPONSE is a plist containing the API response data."
  (when-let* ((choices (plist-get response
                                  :choices))
              (choice (elt choices 0))
              (delta (plist-get choice :delta))
              (content (plist-get delta :content)))
    (decode-coding-string content 'utf-8)))

(defun gpt-commit--stream-insert-response (response info)
  "Insert and format RESPONSE text at a marker.

Argument RESPONSE is a string containing the server's response.

Argument INFO is a property list containing the insertion position and tracking
information."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (inhibit-modification-hooks t))
    (when (and response
               (not (string-empty-p response)))
      (with-current-buffer (marker-buffer start-marker)
        (save-excursion
          (unless tracking-marker
            (goto-char start-marker)
            (let ((msg (buffer-substring-no-properties (line-beginning-position)
                                                       (point))))
              (cond ((string-prefix-p msg response)
                     (setq response (substring response (length msg))))
                    ((string-prefix-p msg (concat response " "))
                     (setq response (substring response (1+ (length msg)))))
                    ((string-prefix-p response msg)
                     (delete-char (- (- (length msg)
                                        (length response))))
                     (setq response ""))))
            (setq tracking-marker (set-marker (make-marker) (point)))
            (set-marker-insertion-type tracking-marker t)
            (plist-put info :tracking-marker tracking-marker))
          (put-text-property 0 (length response) 'gpt-commit 'response
                             response)
          (goto-char tracking-marker)
          (insert
           response))))))

(defun gpt-commit--abort-by-marker (marker)
  "Restore text properties and clean up after aborting a request.

Argument MARKER is a marker object indicating the position in the buffer where
the text properties should be restored."
  (let ((buff
         (when (markerp marker)
           (marker-buffer marker))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (save-excursion
          (gpt-commit-goto-char marker)
          (gpt-commit-restore-text-props))))
    (when-let ((cell (rassq marker gpt-commit--request-url-buffers)))
      (setcdr cell nil))))

(defun gpt-commit-after-change-hook (info)
  "Parse and insert GPT-generated Emacs Lisp documentation.

Argument INFO is a property list containing various request-related data."
  (let ((request-buffer (plist-get info :request-buffer))
        (request-marker (plist-get info :request-marker))
        (buffer (plist-get info :buffer))
        (callback (plist-get info :callback)))
    (when request-buffer
      (with-current-buffer request-buffer
        (when (and (boundp 'url-http-end-of-headers)
                   url-http-end-of-headers)
          (save-match-data
            (save-excursion
              (if request-marker
                  (goto-char request-marker)
                (goto-char url-http-end-of-headers)
                (setq request-marker (point-marker))
                (plist-put info :request-marker request-marker))
              (unless (eolp)
                (beginning-of-line))
              (let ((errored nil))
                (when gpt-commit-debug
                  (setq gpt-commit--debug-data-raw
                        (append gpt-commit--debug-data-raw
                                (list
                                 (list
                                  (buffer-substring-no-properties
                                   (point-min)
                                   (point-max))
                                  (point))))))
                (while (and (not errored)
                            (search-forward "data: " nil t))
                  (let* ((line
                          (buffer-substring-no-properties
                           (point)
                           (line-end-position))))
                    (if (string= line "[DONE]")
                        (progn
                          (when (and (not (plist-get info :done))
                                     (buffer-live-p buffer))
                            (plist-put info :done t)
                            (let ((tracking-marker
                                   (plist-get info
                                              :tracking-marker))
                                  (final-callback
                                   (plist-get info
                                              :final-callback)))
                              (with-current-buffer buffer
                                (save-excursion
                                  (when tracking-marker
                                    (goto-char tracking-marker))
                                  (when final-callback
                                    (funcall
                                     final-callback))
                                  (gpt-commit-remove-text-props))
                                (setq gpt-commit--request-url-buffers
                                      (assq-delete-all
                                       (plist-get info :request-buffer)
                                       gpt-commit--request-url-buffers))
                                (when (symbol-value 'gpt-commit-abort-mode)
                                  (gpt-commit-abort-mode -1)))))
                          (set-marker
                           request-marker
                           (point)))
                      (condition-case _err
                          (let* ((data (gpt-commit--json-parse-string
                                        line 'plist))
                                 (err (plist-get data :error)))
                            (end-of-line)
                            (set-marker
                             request-marker
                             (point))
                            (if err
                                (progn
                                  (setq errored t)
                                  (when err
                                    (message "gpt-commit-error: %s"
                                             (or
                                              (plist-get err
                                                         :message)
                                              err))))
                              (when callback
                                (funcall
                                 callback
                                 data))))
                        (error
                         (setq errored t)
                         (goto-char
                          request-marker))))))))))))))


(defun gpt-commit-stream-request (system-prompt user-prompt &optional
                                                final-callback buffer position)
  "Send GPT stream request with USER-PROMPT and SYSTEM-PROMPT.

Argument SYSTEM-PROMPT is a string representing the system's part of the
conversation.

Argument USER-PROMPT is a string representing the user's part of the
conversation.

Optional argument FINAL-CALLBACK is a function to be called when the request is
completed.

Optional argument BUFFER is the buffer where the output should be inserted. It
defaults to the current buffer.

Optional argument POSITION is the position in the BUFFER where the output should
be inserted. It can be a marker, an integer, or nil. If nil, the current point
or region end is used."
  (let* ((buffer (or buffer (current-buffer)))
         (start-marker
          (cond ((not position)
                 (point-marker))
                ((markerp position) position)
                ((integerp position)
                 (set-marker (make-marker) position buffer))))
         (info (list
                :buffer buffer
                :final-callback final-callback
                :position start-marker))
         (url-request-extra-headers `(("Authorization" .
                                       ,(encode-coding-string
                                         (string-join
                                          `("Bearer"
                                            ,(gpt-commit-get-api-key))
                                          " ")
                                         'utf-8))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data (encode-coding-string
                            (json-encode
                             (list
                              :messages
                              (apply #'vector `((:role "system"
                                                 :content ,(or system-prompt ""))
                                                (:role "user"
                                                 :content ,user-prompt)))
                              :model gpt-commit-model
                              :temperature gpt-commit-gpt-temperature
                              :stream t))
                            'utf-8))
         (request-buffer)
         (callback
          (lambda (response)
            (let ((err (plist-get response :error)))
              (if err
                  (progn
                    (message "gpt-commit-callback err %s"
                             (or
                              (plist-get err
                                         :message)
                              err))
                    (when (buffer-live-p buffer)
                      (let ((start-marker
                             (plist-get info
                                        :position)))
                        (gpt-commit--abort-by-marker start-marker))))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (gpt-commit--stream-insert-response
                     (gpt-commit--get-response-content
                      response)
                     info))))))))
    (plist-put info :callback callback)
    (setq request-buffer
          (url-retrieve
           gpt-commit-gpt-url
           (lambda (status &rest _events)
             (let* ((buff (current-buffer))
                    (err
                     (gpt-commit--retrieve-error status)))
               (if (not err)
                   (when (symbol-value 'gpt-commit-abort-mode)
                     (gpt-commit-abort-mode -1))
                 (run-with-timer 0.5 nil #'gpt-commit--abort-by-url-buffer buff)
                 (message err))))))
    (plist-put info :request-buffer request-buffer)
    (push (cons request-buffer start-marker)
          gpt-commit--request-url-buffers)
    (with-current-buffer buffer
      (gpt-commit-abort-mode 1)
      (add-hook 'kill-buffer-hook #'gpt-commit-abort-current-buffer nil t))
    (with-current-buffer request-buffer
      (add-hook 'after-change-functions
                (lambda (&rest _)
                  (gpt-commit-after-change-hook info))
                nil t))))

(defvar-local gpt-commit--bus nil)

(defun gpt-commit-command-watcher ()
  "Monitor `keyboard-quit' commands and handle GPT documentation aborts."
  (cond ((and gpt-commit--request-url-buffers
              (eq this-command 'keyboard-quit))
         (push this-command gpt-commit--bus)
         (let ((len (length gpt-commit--bus)))
           (cond ((>= len gpt-commit-abort-on-keyboard-quit-count)
                  (message  "gpt-commit: Aborting")
                  (setq gpt-commit--bus nil)
                  (gpt-commit-abort-all))
                 ((< len gpt-commit-abort-on-keyboard-quit-count)
                  (message nil)
                  (message
                   (substitute-command-keys
                    "gpt-commit: Press `\\[keyboard-quit]' %d more times to force interruption.")
                   (- gpt-commit-abort-on-keyboard-quit-count len))))))
        (gpt-commit--bus (setq gpt-commit--bus nil))))

(define-minor-mode gpt-commit-abort-mode
  "Toggle monitoring `keyboard-quit' commands for aborting GPT requests.

Enable `gpt-commit-abort-mode' to monitor and handle `keyboard-quit'
commands for aborting GPT documentation requests.

When active, pressing `\\[keyboard-quit]' multiple times can trigger the
cancellation of ongoing documentation generation processes.

See also custom variable `gpt-commit-abort-on-keyboard-quit-count' for
exact number of `keyboard-quit' presses to abort."
  :lighter " gpt-commit"
  :global nil
  (remove-hook 'pre-command-hook #'gpt-commit-command-watcher 'local)
  (if (not gpt-commit-abort-mode)
      (message (setq gpt-commit--bus nil))
    (add-hook 'pre-command-hook #'gpt-commit-command-watcher nil 'local)
    (message
     (substitute-command-keys
      "gpt-commit: Press `\\[keyboard-quit]' %d more times to force interruption.")
     gpt-commit-abort-on-keyboard-quit-count)))

(defvar url-http-end-of-headers)

(defun gpt-commit-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (with-temp-buffer
    (let ((status (apply #'call-process command nil t nil
                         args)))
      (let ((result (string-trim (buffer-string))))
        (if (zerop status)
            result
          (message result) nil)))))

(defvar-local gpt-commit-request-buffer nil)

(defun gpt-commit-abort-url-retrieve (buff)
  "Cancel the URL retrieval process and kill the associated buffer.

Argument BUFF is a buffer object that represents the buffer to be checked and
potentially killed."
  (when (buffer-live-p buff)
    (message "gpt-commit aborting request")
    (let ((proc (get-buffer-process buff)))
      (when proc
        (delete-process proc))
      (kill-buffer buff))))

(defun gpt-commit-get-response-content (response)
  "Get the content field from the choices array in the message of a RESPONSE."
  (cdr
   (assq 'content
         (cdr
          (assq 'message
                (elt
                 (cdr
                  (assq 'choices
                        response))
                 0))))))

(defun gpt-commit-get-status-error (response)
  "Format and display error messages from `gpt-commit' request responses.

Argument RESPONSE is a list that represents the response from the GPT commit
request."
  (when-let ((err (plist-get response :error)))
    (concat (propertize
             "gpt-commit request error: "
             'face
             'error)
            (mapconcat (apply-partially #'format "%s")
                       (delq nil
                             (list (or
                                    (when-let ((type
                                                (ignore-errors
                                                  (cadr
                                                   err))))
                                      type)
                                    err)
                                   (ignore-errors (caddr
                                                   err))))
                       " "))))

(defun gpt-commit-get-response-error (response)
  "Extract and format error message from a GPT commit RESPONSE.

Argument RESPONSE is a list that represents the response from the GPT commit
request."
  (when-let ((err (cdr-safe (assq 'error response))))
    (concat (propertize
             "gpt-commit request error: "
             'face
             'error)
            (format "%s" (or (cdr-safe (assq 'message err)) err)))))

(defvar url-request-method)
(defvar url-request-data)
(defvar url-request-data)
(defvar url-request-extra-headers)

(defun gpt-commit-doc-gpt-request (messages callback &optional error-callback
                                            model)
  "Send a POST request to the GPT API with specified MESSAGES and callbacks.

Argument MESSAGES is a list of message objects to be sent to the GPT-3 MODEL.

Argument CALLBACK is a function to be called when the GPT-3 MODEL returns a
successful response.

Optional argument ERROR-CALLBACK is a function to be called when the GPT-3 MODEL
returns an error response.
If not provided, no function will be called in case of an error.

Optional argument MODEL is a string representing the GPT-3 MODEL to be used for
the request.
If not provided, the default MODEL specified in `gpt-commit-model' will be
used."
  (gpt-commit-abort-url-retrieve gpt-commit-request-buffer)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (gpt-commit--api-key)))))
         (data `((model . ,(or model gpt-commit-model))
                 (messages . ,messages)))
         (payload (encode-coding-string (json-serialize data) 'utf-8))
         (url-request-data
          payload))
    (setq gpt-commit-request-buffer
          (url-retrieve gpt-commit-api-url
                        (lambda (status &rest _)
                          (if-let ((err
                                    (gpt-commit-get-status-error status)))
                              (and error-callback (funcall error-callback err status))
                            (let* ((response
                                    (progn (goto-char url-http-end-of-headers)
                                           (gpt-commit-json-read-buffer 'alist)))
                                   (response-err (gpt-commit-get-response-error
                                                  response)))
                              (if response-err
                                  (and error-callback
                                       (funcall error-callback response-err
                                                status))
                                (funcall callback
                                         (gpt-commit-get-response-content
                                          response))))))))))

(defun gpt-commit--run-with-fallback (request-data callback buffer)
  "Execute a GPT request and retry with a fallback model if an error occurs.

Argument REQUEST-DATA is a data structure that contains the information needed
to make the request.
Argument CALLBACK is a function that will be called when the request is
completed.
Argument BUFFER is a BUFFER object where the request's response will be stored."
  (let ((success-callback (lambda (&rest args)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (apply callback args)))))
        (error-callback (lambda (error-thrown &rest _)
                          (message error-thrown))))
    (gpt-commit-doc-gpt-request
     request-data
     success-callback
     error-callback
     gpt-commit-model)))

(defun gpt-commit--improve-message (msg buffer callback)
  "Improve a commit message MSG in BUFFER and pass it to the CALLBACK."
  (let ((messages `[((role . "system")
                     (content . ,gpt-commit-improve-system-prompt))
                    ((role . "user")
                     (content . ,msg))]))
    (gpt-commit--run-with-fallback messages callback buffer)))

(defun gpt-commit-generate-message (msg buffer callback)
  "Generate a complete commit message based on user input and git diff output.

Argument MSG is a string that represents the `user-supplied' incomplete commit
message.
Argument BUFFER is a buffer where in which function CALLBACK
will be called when the request is completed."
  (let* ((changes (gpt-commit-call-process "git" "diff" "--cached"))
         (user-prompt (format "%s\n\ngit diff output:\n```%s```"
                              msg changes))
         (messages `[((role . "system")
                      (content . ,(or (nth gpt-commit-curr-prompt-idx
                                       gpt-commit-system-prompts)
                                   gpt-commit-system-prompt-en)))
                     ((role . "user")
                      (content . ,user-prompt))]))
    (gpt-commit--run-with-fallback messages callback buffer)))



(defun gpt-commit-read-type ()
  "Prompt user to select the type of change they're committing."
  (let* ((alist gpt-commit-types-alist)
         (max-len (apply #'max (mapcar
                                (lambda (it)
                                  (length (car it)))
                                gpt-commit-types-alist)))
         (annotf (lambda (str)
                   (let ((rule-alist (cdr (assoc-string str alist)))
                         (prefix (make-string (- max-len (length str)) ?\ ))
                         (used-width)
                         (res))
                     (dotimes (i (length gpt-commit-annotation-spec-alist))
                       (pcase-let* ((`(,key ,format-str ,width)
                                     (nth i gpt-commit-annotation-spec-alist))
                                    (value (alist-get key rule-alist))
                                    (label))
                         (if
                             (< i (1- (length gpt-commit-annotation-spec-alist)))
                             (setq used-width (+ (or used-width 0) width))
                           (setq width (- (window-width) (+ used-width max-len) 10)))
                         (setq label (if (> (string-width str) width)
                                         (truncate-string-to-width
                                          (format format-str (or value
                                                                 ""))
                                          width
                                          0 nil
                                          t)
                                       (format format-str (or value
                                                              ""))))
                         (setq res (concat
                                    (or res "")
                                    (propertize " "
                                                'display
                                                (list 'space :align-to
                                                      used-width))
                                    label))))
                     (concat prefix res)))))
    (completing-read "Select the type of change that you're committing: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action alist str pred))))))

(defun gpt-commit--retrieve-issue-key-from-branch (branch)
  "Retrieve issue key from BRANCH."
  (with-temp-buffer (insert branch)
                    (when (re-search-backward
                           "\\(^\\|[_/-]\\)\\(\\([a-z]+[-_][0-9]+\\)\\)\\($\\|[-_/]\\)"
                           nil t 1)
                      (match-string-no-properties 3))))

(defun gpt-commit-retrieve-issue-key-from-branch ()
  "Retrieve jira issue from STR."
  (when-let ((branch (gpt-commit-call-process "git" "symbolic-ref" "--short"
                                              "HEAD")))
    (gpt-commit--retrieve-issue-key-from-branch branch)))

(defun gpt-commit-buffer-message ()
  "Return commit message from current buffer."
  (let ((flush (concat "^" comment-start))
        (str (buffer-substring-no-properties (point-min)
                                             (point-max))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward (concat flush " -+ >8 -+$") nil t)
        (delete-region (line-beginning-position)
                       (point-max)))
      (goto-char (point-min))
      (flush-lines flush)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (setq str (buffer-string)))
    (and (not (string-match "\\`[ \t\n\r]*\\'" str))
         (progn
           (when (string-match "\\`\n\\{2,\\}" str)
             (setq str (replace-match "\n" t t str)))
           (when (string-match "\n\\{2,\\}\\'" str)
             (setq str (replace-match "\n" t t str)))
           str))))

(defun gpt-commit-current-commit-type ()
  "Check and return the current commit type from the git commit message."
  (when-let ((msg (gpt-commit-buffer-message))
             (re (concat "^" (regexp-opt (mapcar #'car
                                                 gpt-commit-types-alist)))))
    (and (string-match-p re msg)
         (with-temp-buffer (insert msg)
                           (goto-char (point-min))
                           (when (re-search-forward re nil t 1)
                             (match-string-no-properties 0))))))

(defun gpt-commit-get-commit-type-regex ()
  "Generate a regex to match the start of commit types."
  (concat "^" (regexp-opt (mapcar #'car
                                  gpt-commit-types-alist))))

;;;###autoload
(defun gpt-commit-toggle-commit-type ()
  "Toggle between different commit types in a `git-commit-mode'."
  (interactive)
  (let* ((current (gpt-commit-current-commit-type))
         (next
          (if-let ((cell (assoc-string current gpt-commit-types-alist)))
              (caadr (member cell gpt-commit-types-alist))
            (caar gpt-commit-types-alist))))
    (gpt-commit-update-commit-type next)
    (when transient-current-command
      (transient-setup transient-current-command))))

;;;###autoload
(defun gpt-commit-update-issue-key (issue-key)
  "Add or update the ISSUE-KEY from the current branch into the commit message."
  (interactive (list
                (gpt-commit-retrieve-issue-key-from-branch)))
  (when issue-key
    (let* ((msg (gpt-commit-buffer-message))
           (type-re (concat "^" (regexp-opt (mapcar #'car
                                                    gpt-commit-types-alist)))))
      (cond ((not msg)
             (goto-char (point-min))
             (insert (format "%s: " issue-key)))
            ((string-match-p (concat type-re ":") msg)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward type-re nil t 1)
               (insert (format "(%s)" issue-key))))
            ((string-match-p (concat type-re "[(]\\([^)]+\\)[)]") msg)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward type-re nil t 1)
               (when (re-search-forward "[(]\\([^)]+\\)[)]" nil t 1)
                 (replace-match issue-key nil nil nil 1))))))))

;;;###autoload
(defun gpt-commit-update-commit-type (new-type)
  "Update commit message with a new commit type.

Argument NEW-TYPE is a string representing the new commit type to be applied."
  
  (interactive (list (gpt-commit-read-type)))
  (let ((msg (gpt-commit-buffer-message))
        (type-re (concat "^" (regexp-opt (mapcar #'car
                                                 gpt-commit-types-alist)))))
    (cond ((and (or (not new-type)
                    (string-empty-p new-type))
                (string-match-p (concat type-re "[(:]") msg))
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward type-re nil t 1)
               (replace-match "" nil nil nil 0))
             (if (looking-at ":")
                 (delete-char 1)
               (when (looking-at "[(]\\([^)]+\\)[)]")
                 (re-search-forward "[(]\\([^)]+\\)[)]" nil t 1)
                 (let ((str (match-string-no-properties 1)))
                   (replace-match str nil nil nil 0))))))
          ((not msg)
           (goto-char (point-min))
           (insert (format "%s: " new-type)))
          ((string-match-p (concat type-re "[(:]") msg)
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward type-re nil t 1)
               (replace-match new-type nil nil nil 0))))
          ((string-match-p "^[[:upper:]]+[-_][[:digit:]]+" msg)
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward "^[[:upper:]]+[-_][[:digit:]]+" nil t 1)
               (let ((str (match-string-no-properties 0))
                     (beg (match-beginning 0))
                     (end (match-end 0)))
                 (replace-region-contents beg end
                                          (lambda ()
                                            (format "%s(%s)" new-type
                                                    str)))))))
          (t (save-excursion
               (goto-char (point-min))
               (insert (format "%s: " new-type)))))))

(defun gpt-commit-get-commit-msg-end ()
  "Find the end position of the commit message in a buffer."
  (save-excursion
    (if (re-search-forward (concat "^" comment-start) nil t 1)
        (progn (forward-char -1)
               (forward-line -1))
      (goto-char (point-max)))
    (while (and (not (bobp))
                (looking-at "\n"))
      (forward-line -1))
    (line-end-position)))

;;;###autoload
(defun gpt-commit-stream-abort (buff)
  "Abort stream processes associated with a buffer BUFF.

Argument BUFF is the buffer associated with the process to be aborted."
  (interactive (list (current-buffer)))
  (gpt-commit--abort-by-url-buffer buff))

;;;###autoload
(defun gpt-commit-improve-message ()
  "Analyze and regenerate an improved commit message and don't send git diff.

If `gpt-commit-use-stream' is non nil, use curl for streaming response.
To terminate it use command `gpt-commit-stream-abort'.

Unlike `gpt-commit-message', this command does not send the git diff output,
but focuses solely on the current commit message.

It generates an enhanced conventional commit message based on either the primary
or the fallback model. If a commit type prefix (feat:, fix:, etc.) exists, this
function preserves it and improves the remaining part.

Customize `gpt-commit-improve-system-prompt' to modify the system prompt and
`gpt-commit-types-alist' to specify conventional commit types.

Before using, set OpenAI API key
`gpt-commit-api-key' and GPT model - `gpt-commit-model'."
  (interactive)
  (let* ((end (gpt-commit-get-commit-msg-end))
         (msg
          (buffer-substring-no-properties (point-min)
                                          end)))
    (when (or (not msg)
              (string-empty-p msg))
      (user-error "No commit message to improve"))
    (goto-char end)
    (if gpt-commit-use-stream
        (gpt-commit-stream-request gpt-commit-improve-system-prompt msg)
      (let* ((buffer (current-buffer))
             (regex (concat (gpt-commit-get-commit-type-regex)
                            "\\([(][^)]+[)]\\)?"
                            ":[\s\t]*"))
             (prefix
              (and msg
                   (with-temp-buffer
                     (insert msg)
                     (goto-char (point-min))
                     (when (re-search-forward regex nil
                                              t 1)
                       (match-string-no-properties 0))))))
        (gpt-commit--improve-message
         (if prefix
             (substring-no-properties msg (length prefix))
           msg)
         buffer
         (lambda (commit-message)
           (when commit-message
             (goto-char (point-min))
             (when prefix
               (re-search-forward ":" nil t 1)
               (if (looking-at "[\s\t]")
                   (skip-chars-forward "\s\t")
                 (insert "\s")))
             (delete-region (point)
                            (gpt-commit-get-commit-msg-end))
             (save-excursion
               (insert (concat commit-message "\n\n")))
             (downcase-word 1)
             (goto-char (line-end-position))
             (when (looking-at "\n[^\n#]")
               (insert "\n")))))))))

;;;###autoload
(defun gpt-commit-message ()
  "Generate and insert a commit message using GPT.

If `gpt-commit-use-stream' is non nil, use curl for streaming response.
To terminate it use command `gpt-commit-stream-abort'.

This command is designed for `git-commit-mode'.

It analyzes both the changes and the current commit message, then
generates a conventional commit message using the primary model. If
the primary model fails, it uses the fallback one.

`gpt-commit-system-prompt-en' allows modification of the system prompt and
`gpt-commit-types-alist' for specifying commit types.

Prior to usage, setup OpenAI API key `gpt-commit-api-key'
and GPT model `gpt-commit-model'."
  (interactive)
  (let* ((end (gpt-commit-get-commit-msg-end))
         (msg
          (buffer-substring-no-properties (point-min)
                                          end)))
    (goto-char (gpt-commit-get-commit-msg-end))
    (if gpt-commit-use-stream
        (gpt-commit-stream-request
         (or (nth gpt-commit-curr-prompt-idx gpt-commit-system-prompts)
             gpt-commit-system-prompt-en)
         (format "%s\n\ngit diff output:\n%s"
                 msg
                 (gpt-commit-call-process
                  "git" "diff" "--cached")))
      (gpt-commit-generate-message
       msg
       (current-buffer)
       (lambda (commit-message)
         (if (bobp)
             (insert commit-message)
           (replace-region-contents (point-min)
                                    (point)
                                    (lambda () commit-message))))))))



(defun gpt-commit--index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

;;;###autoload (autoload 'gpt-commit-menu "gpt-commit" nil t)
(transient-define-prefix gpt-commit-menu ()
  "Activate a menu for Git commit operations."
  :refresh-suffixes t
  [:description (lambda ()
                  (with-temp-buffer
                    (insert (nth gpt-commit-curr-prompt-idx
                                 gpt-commit-system-prompts))
                    (fill-region (point-min)
                                 (point-max))
                    (buffer-string)))
   ("p" "Previous system prompt"
    (lambda ()
      (interactive)
      (setq gpt-commit-curr-prompt-idx
            (gpt-commit--index-switcher -1
                                        gpt-commit-curr-prompt-idx
                                        gpt-commit-system-prompts)))
    :transient t)
   ("n" "Next system prompt"
    (lambda ()
      (interactive)
      (setq gpt-commit-curr-prompt-idx
            (gpt-commit--index-switcher 1
                                        gpt-commit-curr-prompt-idx
                                        gpt-commit-system-prompts)))
    :transient t)
   ("SPC" "Add system prompt"
    (lambda ()
      (interactive)
      (string-edit
       "New system prompt: "
       ""
       (lambda (edited)
         (add-to-list 'gpt-commit-system-prompts edited)
         (when-let ((idx (seq-position gpt-commit-system-prompts edited)))
           (setq gpt-commit-curr-prompt-idx idx))
         (transient-setup 'gpt-commit-menu))
       :abort-callback (lambda ())))
    :transient nil)
   ("e" "Edit system prompt"
    (lambda ()
      (interactive)
      (string-edit
       "Edit system prompt: "
       (nth gpt-commit-curr-prompt-idx
            gpt-commit-system-prompts)
       (lambda (edited)
         (setf (nth gpt-commit-curr-prompt-idx gpt-commit-system-prompts)
               edited)
         (transient-setup 'gpt-commit-menu))
       :abort-callback (lambda ())))
    :transient nil)
   ("D" "Delete current system prompt"
    (lambda ()
      (interactive)
      (setq gpt-commit-system-prompts
            (remove (nth gpt-commit-curr-prompt-idx gpt-commit-system-prompts)
                    gpt-commit-system-prompts)))
    :transient t)
   ("C-x C-w" "Save prompts"
    (lambda ()
      (interactive)
      (customize-save-variable
       'gpt-commit-system-prompts
       gpt-commit-system-prompts))
    :transient t)
   ("c" "GPT Commit" gpt-commit-message)]
  ["Commit Type"
   ("t" gpt-commit-toggle-commit-type
    :description (lambda ()
                   (let ((current (gpt-commit-current-commit-type)))
                     (mapconcat
                      (pcase-lambda (`(,key . ,_cell))
                        (propertize (format "%s" (substring-no-properties key))
                                    'face
                                    (if (and current
                                             (string= current (substring-no-properties
                                                               key)))
                                        'transient-value
                                      'transient-inactive-value)))
                      gpt-commit-types-alist
                      (propertize "|" 'face 'transient-inactive-value)))))
   ("u" gpt-commit-update-commit-type
    :description (lambda ()
                   (or
                    (when-let* ((current
                                 (gpt-commit-current-commit-type))
                                (cell
                                 (assoc-string
                                  current
                                  gpt-commit-types-alist)))
                      (string-join
                       (delq nil
                             (list (alist-get
                                    'emoji
                                    cell)
                                   (alist-get
                                    'description
                                    cell)))
                       " "))
                    "None")))
   ("f" "GPT improve" gpt-commit-improve-message
    :inapt-if-not gpt-commit-buffer-message)]
  ["Issue Key"
   ("i" gpt-commit-update-issue-key
    :inapt-if-not gpt-commit-retrieve-issue-key-from-branch
    :description (lambda ()
                   (concat "Insert issue key ("
                           (or (gpt-commit-retrieve-issue-key-from-branch)
                               "None")
                           ")")))]
  ["Git commit"
   :if (lambda ()
         (bound-and-true-p git-commit-mode))
   ("M-p" "Previous message" git-commit-prev-message
    :transient t
    :if (lambda ()
          (bound-and-true-p git-commit-mode)))
   ("M-n" "Next message" git-commit-next-message
    :transient t
    :if (lambda ()
          (bound-and-true-p git-commit-mode)))
   ("T" "Insert a commit message trailer" git-commit-insert-trailer)
   ("S" "Save" git-commit-save-message)
   ("L" "Cancel" with-editor-cancel)
   ("F" "Commit" with-editor-finish)]
  (interactive)
  (add-to-list 'gpt-commit-system-prompts gpt-commit-system-prompt-en)
  (when (not (nth gpt-commit-curr-prompt-idx gpt-commit-system-prompts))
    (setq gpt-commit-curr-prompt-idx 0))
  (transient-setup #'gpt-commit-menu))

(provide 'gpt-commit)
;;; gpt-commit.el ends here