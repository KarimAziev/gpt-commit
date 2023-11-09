;;; gpt-commit.el --- Commit messages with GPT -*- lexical-binding: t; -*-

;; Author: Youngwook Kim <youngwook.kim@gmail.com>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gpt-commit
;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "28.1") (magit "3.3.0") (transient "0.4.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: convenience

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

(defcustom gpt-commit-use-stream (and (executable-find "curl")
                                      t)
  "Whether to use `curl' for streaming."
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

(defvar gpt-commit--process-alist nil
  "Alist of active curl requests.")

(defvar gpt-commit--debug nil)

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

(defun gpt-commit--stream-sentinel (process _status)
  "Handle PROCESS completion and update buffers.

Argument PROCESS is the subprocess associated with the sentinel.

Argument _STATUS is a string representing the change in the PROCESS's state."
  (let ((proc-buf (process-buffer process)))
    (when gpt-commit--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gpt-doc-error*" 'show)))
    (let* ((info (alist-get process gpt-commit--process-alist))
           (gpt-doc-buffer (plist-get info :buffer))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position))
           (http-status (plist-get info :http-status))
           (http-msg (plist-get info :status)))
      (if (equal http-status "200")
          (with-current-buffer (marker-buffer start-marker)
            (pulse-momentary-highlight-region (+ start-marker 2)
                                              tracking-marker))
        (with-current-buffer proc-buf
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let*
              ((`(,_ . ,header-size)
                (read (current-buffer)))
               (response
                (progn (goto-char header-size)
                       (condition-case nil (gpt-commit-json-read-buffer 'plist)
                         (json-readtable-error 'json-read-error)))))
            (cond
             ((plist-get response :error)
              (let* ((error-plist (plist-get response :error))
                     (error-msg (plist-get error-plist :message))
                     (error-type (plist-get error-plist :type)))
                (message "GPT-commit error: (%s) %s" http-msg error-msg)
                (setq http-msg (concat "("  http-msg ") " (string-trim error-type)))))
             ((eq response 'json-read-error)
              (message "GPT-commit error (%s): Malformed JSON in response."
                       http-msg))
             (t (message "GPT-commit error (%s): Could not parse HTTP response."
                         http-msg)))))
        (message (format " Response Error: %s" http-msg)))
      (with-current-buffer gpt-doc-buffer
        (message "gpt-commit-post-response-hook")
        (run-hooks 'gpt-commit-post-response-hook)))
    (setf (alist-get process gpt-commit--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))


(defun gpt-commit--stream-filter (process output)
  "Insert HTTP response data into a buffer at a specified position.

Argument PROCESS is the process that generates the output.

Argument OUTPUT is the output generated by the process."
  (let* ((proc-info (alist-get process gpt-commit--process-alist)))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process)
                    (point)))
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position)
                               (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)"
                                            http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))))
        (when (with-current-buffer (plist-get proc-info :buffer)
                (or buffer-read-only
                    (get-char-property (plist-get proc-info :position)
                                       'read-only)))
          (message
           "Buffer is read only, displaying reply in buffer \"*GPT-DOC response*\"")
          (display-buffer
           (with-current-buffer (get-buffer-create "*GPT-DOC response*")
             (goto-char (point-max))
             (move-marker (plist-get proc-info :position)
                          (point)
                          (current-buffer))
             (current-buffer))
           '((display-buffer-reuse-window
              display-buffer-pop-up-window)
             (reusable-frames . visible))))
        (let ((status (plist-get proc-info :http-status))
              (position (plist-get proc-info :position)))
          (when (and (equal status "200"))
            (with-current-buffer (marker-buffer position)
              (run-hooks 'gpt-commit-pre-response-hook)))))
      (when-let ((http-msg (plist-get proc-info :status))
                 (http-status (plist-get proc-info :http-status)))
        (when (equal http-status "200")
          (funcall (or (plist-get proc-info :callback)
                       #'gpt-commit--stream-insert-response)
                   (let* ((json-object-type 'plist)
                          (content-strs))
                     (condition-case nil
                         (while (re-search-forward "^data:" nil t)
                           (save-match-data
                             (unless (looking-at " *\\[DONE\\]")
                               (when-let* ((response (gpt-commit-json-read-buffer
                                                      'plist))
                                           (delta
                                            (plist-get (elt
                                                        (plist-get response
                                                                   :choices)
                                                        0)
                                                       :delta))
                                           (content (plist-get delta :content)))
                                 (push content content-strs)))))
                       (error
                        (goto-char (match-beginning 0))))
                     (apply #'concat (nreverse content-strs)))
                   proc-info))))))

(defun gpt-commit-insert-with-fill (response)
  "Insert RESPONSE into buffer, wrapping text if width exceeds 80 characters.
Argument RESPONSE is the string to be inserted into the buffer."
  (let* ((curr-content
          (let ((beg (line-beginning-position)))
            (buffer-substring-no-properties beg (point))))
         (wid (string-width (concat
                             curr-content
                             response))))
    (if (not (>= wid 80))
        (insert response)
      (insert response)
      (fill-region-as-paragraph (line-beginning-position)
                                (point)))))

(defun gpt-commit--stream-insert-response (response info)
  "Insert RESPONSE into the buffer associated with start-marker in INFO.

Argument RESPONSE is a string containing the server's response to be inserted
into the buffer.

Argument INFO is a property list containing insertion information such as
position, tracking marker, transformer function, and inserter function."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer))
        (insert-fn (or (plist-get info :inserter) #'insert))
        (first-completion))
    (when response
      (with-current-buffer (marker-buffer start-marker)
        (save-excursion
          (unless tracking-marker
            (setq first-completion t)
            (goto-char start-marker)
            (setq tracking-marker (set-marker (make-marker) (point)))
            (set-marker-insertion-type tracking-marker t)
            (plist-put info :tracking-marker tracking-marker))
          (put-text-property 0 (length response) 'gpt-commit 'response response)
          (goto-char tracking-marker)
          (when transformer
            (setq response (funcall transformer response)))
          (when first-completion
            (let ((msg (gpt-commit-buffer-message)))
              (if (and msg (string-prefix-p msg response))
                  (setq response (substring response (length msg)))
                (delete-region (point-min)
                               (point)))))
          (funcall insert-fn response))))))

(defun gpt-commit--api-key ()
  "Retrieve the OpenAI API key, either directly or from a function."
  (if (functionp
       'gpt-commit-api-key)
      (funcall
       gpt-commit-api-key)
    gpt-commit-api-key))

(defun gpt-commit--get-curl-stream-args (request-data token)
  "Generate curl arguments for streaming a POST request with given data and TOKEN.

Argument REQUEST-DATA is a data structure that will be converted to a JSON
string and sent as part of the HTTP request body.

Argument TOKEN is a string that will be used to format a curl command argument."
  (require 'json)
  (append
   (list "--location" "--silent" "--compressed" "--disable"
         (format "-X%s" "POST")
         (format "-w(%s . %%{size_header})" token)
         (format "-m%s" 60)
         "-D-"
         (format "-d%s" (encode-coding-string
                         (let ((json-object-type 'plist))
                           (json-encode
                            request-data))
                         'utf-8)))
   (seq-map (lambda (header)
              (format "-H%s: %s" (car header)
                      (cdr header)))
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer "
                                          (gpt-commit--api-key)))))
   (list gpt-commit-api-url)))

(defun gpt-commit-stream-request (system-prompt user-prompt &optional buffer
                                                position)
  "Send a streaming request to the GPT API and handle the response.

Argument SYSTEM-PROMPT is a string that represents the system's prompt.

Argument USER-PROMPT is a string that represents the user's prompt.

Optional argument BUFFER is the buffer where the request will be committed. If
not provided, the current BUFFER is used.

Optional argument POSITION is the position in the BUFFER where the request will
be committed. It can be a marker or an integer. If not provided, the point
marker or the end of the region (if a region is active) is used."
  (let* ((buffer (or buffer (current-buffer)))
         (start-marker
          (cond ((null position)
                 (if (use-region-p)
                     (set-marker (make-marker)
                                 (region-end))
                   (point-marker)))
                ((markerp position) position)
                ((integerp position)
                 (set-marker (make-marker) position buffer))))
         (request-data `(:model ,gpt-commit-model
                                :messages [(:role "system"
                                                  :content ,system-prompt)
                                           (:role "user"
                                                  :content ,user-prompt)]
                                :stream t
                                :temperature ,gpt-commit-gpt-temperature))
         (token (md5 (format "%s%s%s%s" (random)
                             (emacs-pid)
                             (user-full-name)
                             (recent-keys))))
         (args (gpt-commit--get-curl-stream-args request-data token))
         (process
          (apply #'start-process "gpt-commit-curl"
                 (generate-new-buffer "*gpt-commit-curl*") "curl" args))
         (info (list
                :buffer buffer
                :token token
                :position start-marker
                :callback
                #'gpt-commit--stream-insert-response)))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'gpt-commit--stream-abort-current-buffer nil
                t))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process gpt-commit--process-alist) info)
      (set-process-sentinel process #'gpt-commit--stream-sentinel)
      (set-process-filter process #'gpt-commit--stream-filter))))
;;

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
                      (content . ,gpt-commit-system-prompt-en))
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

(defun gpt-commit--stream-abort (buff)
  "Abort the associated with the buffer BUFF stream processes.

Argument BUFF is the buffer associated with the process to be aborted."
  (dolist (proc-attrs gpt-commit--process-alist)
    (when (eq (plist-get (cdr proc-attrs) :buffer) buff)
      (when-let ((proc (car proc-attrs)))
        (setf (alist-get proc gpt-commit--process-alist nil 'remove) nil)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (message "gpt-doc: Aborted request in buffer %S" (buffer-name buff))))))

(defun gpt-commit--stream-abort-current-buffer ()
  "Abort stream processes in current buffer."
  (gpt-commit--stream-abort (current-buffer)))


;;;###autoload
(defun gpt-commit-stream-abort (buff)
  "Abort stream processes associated with a buffer BUFF.

Argument BUFF is the buffer associated with the process to be aborted."
  (interactive (list (current-buffer)))
  (gpt-commit--stream-abort buff))

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
         gpt-commit-system-prompt-en
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

;;;###autoload (autoload 'gpt-commit-menu "gpt-commit" nil t)
(transient-define-prefix gpt-commit-menu ()
  "Activate a menu for Git commit operations."
  ["Cycle"
   :if (lambda ()
         (bound-and-true-p git-commit-mode))
   ("p" "Previous message" git-commit-prev-message :transient t)
   ("n" "Next message" git-commit-next-message :transient t)]
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
   ("o" gpt-commit-update-commit-type
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
   ("c" "GPT Commit" gpt-commit-message)
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
  ["Insert"
   :if (lambda ()
         (bound-and-true-p git-commit-mode))
   ("a" "Ack" git-commit-ack)
   ("O" "Sign-Off" git-commit-signoff)
   ("b" "Modified-by" git-commit-modified)
   ("T" "Tested-by" git-commit-test)
   ("r" "Reviewed-by" git-commit-review)
   ("C" "CC (mentioning someone)" git-commit-cc)
   ("e" "Reported" git-commit-reported)
   ("s" "Suggested" git-commit-suggested)
   ("-" "Co-authored-by" git-commit-co-authored)
   ("d" "Co-developed-by" git-commit-co-developed)]
  ["Do"
   :if (lambda ()
         (bound-and-true-p git-commit-mode))
   ("v" "Save" git-commit-save-message)
   ("l" "Cancel" with-editor-cancel)
   ("m" "Commit" with-editor-finish)])

(provide 'gpt-commit)
;;; gpt-commit.el ends here