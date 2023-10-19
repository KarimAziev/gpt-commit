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
;; With GPT-Commit, you no longer need to spend time crafting commit
;; messages manually.  It analyzes the changes in your Git repository and
;; generates meaningful commit messages automatically, ensuring
;; consistent and descriptive commit logs.
;;
;; Features:
;; - Automatic generation of conventional commit messages
;; - Integration with Git and Magit for seamless workflow
;; - Easy configuration and customization
;;
;; GPT-Commit streamlines the commit process and promotes best practices
;; for commit message formatting.  By using consistent commit messages,
;; you can enhance project clarity, facilitate collaboration, and improve
;; the overall maintainability of your codebase.
;;
;; (require 'gpt-commit)
;; (require 'git-commit)
;; (setq gpt-commit-openai-key "YOUR_OPENAI_API_KEY")
;; (setq gpt-commit-model-name "gpt-4")
;; (setq gpt-commit-fallback-model "gpt-3.5-turbo-16k")
;; (define-key git-commit-mode-map (kbd "C-c *") #'gpt-commit-message)


;;; Code:

(require 'magit)
(require 'transient)

(defcustom gpt-commit-model-name "gpt-4"
  "Model name to use for GPT chat completions."
  :group 'gpt-commit
  :type 'string)

(defcustom gpt-commit-fallback-model "gpt-3.5-turbo-16k"
  "Fallback model name if `gpt-commit-model-name' is failed."
  :group 'gpt-commit
  :type '(radio
          (string :tag "Model" "gpt-3.5-turbo-16k")
          (const :tag "None" nil)))

(defcustom gpt-commit-api-url "https://api.openai.com/v1/chat/completions"
  "API endpoint for GPT chat completions."
  :group 'gpt-commit
  :type 'string)

(defcustom gpt-commit-openai-key ""
  "API key for the OpenAI.
Can also be a function of no arguments that returns an API key (more secure)."
  :group 'gpt-commit
  :type '(radio
          (string :tag "API key")
          (function :tag "Function that returns the API key")))



(defcustom gpt-commit-system-prompt-en "Based on the user-supplied incomplete commit message and the output from `git diff --cached`, your task is to generate a completed, conventional commit message accurately encompassing the changes. Ensure that your response strictly contains the refined commit message, without including any extraneous information. Fill text lines to be no longer than 70."
  "Prompt (directive) for ChatGPT to generate commit message."
  :type 'string
  :group 'gpt-commit)

(defcustom gpt-commit-improve-system-prompt "The user will feed you a commit message. Your job is to evaluate the grammar and refine it if necessary. Make sure to preserve the original meaning and maintain the context of the message. The refined message should remain the only content in your response, and ensure that the lines do not surpass 70 characters in length."
  "Prompt (directive) for ChatGPT to improve commit message."
  :type 'string
  :group 'gpt-commit)

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
                                     (emoji . "🛠"))
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
                                     (emoji . "🗑")))
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

(defcustom gpt-commit-annotation-spec-alist '((emoji "️%s" 15)
                                              (description "%s" 80))
  "Alist of symbol, format string and width for displaying a GitHub repository."
  :group 'gpt-commit
  :type '(alist
          :key-type symbol
          :value-type (list
                       (string :tag "Column Name" "%s")
                       (integer :tag "Column Width" 20))))

(defun gpt-commit-parse-response (data)
  "Parse the GPT response DATA."
  (let* ((choices (cdr (assoc 'choices data)))
         (choice (elt choices 0))
         (message (assoc 'message choice))
         (content (cdr (assoc 'content message))))
    (decode-coding-string content 'utf-8)))


(defvar url-http-end-of-headers)
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
If not provided, the default MODEL specified in `gpt-commit-model-name' will be
used."
  (require 'url)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (if (functionp
                                                       'gpt-commit-openai-key)
                                                      (funcall
                                                       gpt-commit-openai-key)
                                                    gpt-commit-openai-key)))))
         (data `((model . ,(or model gpt-commit-model-name))
                 (messages . ,messages)))
         (payload (encode-coding-string (json-serialize data) 'utf-8))
         (url-request-data
          payload))
    (url-retrieve gpt-commit-api-url
                  (lambda (status &rest _)
                    (if (plist-get status :error)
                        (funcall error-callback (plist-get status :error) status)
                      (let ((response (gpt-commit--json-parse-string
                                       (buffer-substring-no-properties
                                        url-http-end-of-headers
                                        (point-max)))))
                        (if (assoc 'error response)
                            (funcall error-callback
                                     (cdr
                                      (assoc 'message
                                             (cdr
                                              (assoc
                                               'error
                                               response))))
                                     status)
                          (funcall callback
                                   (cdr
                                    (assq 'content
                                          (cdr
                                           (assq 'message
                                                 (elt
                                                  (cdr
                                                   (assq 'choices
                                                         response))
                                                  0)))))))))))))



(defun gpt-commit-generate-message (msg callback)
  "Generate a commit message MSG using GPT and pass it to the CALLBACK."
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n"))
         (user-prompt (format "%s\n\ngit diff output:\n```%s```"
                              msg changes))
         (messages `[((role . "system")
                      (content . ,gpt-commit-system-prompt-en))
                     ((role . "user")
                      (content . ,user-prompt))]))
    (if (and gpt-commit-fallback-model
             (not (equal gpt-commit-fallback-model gpt-commit-model-name)))
        (gpt-commit-doc-gpt-request messages callback
                                    (lambda (error-thrown data)
                                      (message
                                       "GPT %s Error: %s %s, retrying with %s"
                                       gpt-commit-model-name
                                       error-thrown data
                                       gpt-commit-fallback-model)
                                      (gpt-commit-doc-gpt-request
                                       messages
                                       callback
                                       nil
                                       gpt-commit-fallback-model))
                                    gpt-commit-model-name)
      (gpt-commit-doc-gpt-request messages callback
                                  nil gpt-commit-model-name))))



(defun gpt-commit-read-type ()
  "Prompt user to select the type of change they're committing."
  (let* ((alist gpt-commit-types-alist)
         (max-len (apply #'max (mapcar
                                (lambda (it)
                                  (length (car it)))
                                gpt-commit-types-alist)))
         (annotf (lambda (str)
                   (let ((rule-alist (cdr (assoc-string str alist)))
                         (prefix (make-string (- max-len (length str)) ?\ )))
                     (concat prefix " "
                             (mapconcat
                              (pcase-lambda (`(,key ,format-str ,width))
                                (let ((value (alist-get key rule-alist)))
                                  (truncate-string-to-width
                                   (format format-str (or value
                                                          ""))
                                   width
                                   0 nil
                                   t)))
                              gpt-commit-annotation-spec-alist))))))
    (completing-read "Select the type of change that you're committing:"
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action alist str pred))))))

(defun gpt-commit--retrieve-issue-key-from-branch ()
  "Retrieve jira issue from STR."
  (let ((branch (magit-get-current-branch)))
    (let ((re "[[:upper:]]+[-_][[:digit:]]+"))
      (when (string-match-p re branch)
        (replace-regexp-in-string
         (concat ".*?\\(" re "\\).*")
         "\\1"
         branch)))))

(defun gpt-commit-current-commit-type ()
  "Check and return the current commit type from the git commit message."
  (when-let ((msg (git-commit-buffer-message))
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
                (gpt-commit--retrieve-issue-key-from-branch)))
  (when issue-key
    (let* ((msg (git-commit-buffer-message))
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
            ((string-match-p (concat type-re "[(]\\([^)]+]\\)[)]") msg)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward type-re nil t 1)
               (when (re-search-forward "[(]\\([^)]+]\\)[)]" nil t 1)
                 (replace-match issue-key nil nil nil 1))))))))

;;;###autoload
(defun gpt-commit-update-commit-type (new-type)
  "Insert or replace the commit type with NEW-TYPE in a Git commit message."
  (interactive (list (gpt-commit-read-type)))
  (let ((msg (git-commit-buffer-message))
        (type-re (concat "^" (regexp-opt (mapcar #'car
                                                 gpt-commit-types-alist)))))
    (cond ((and (or (not new-type)
                    (string-empty-p new-type))
                (string-match-p (concat type-re "[(:]") msg))
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward (concat type-re "[(:]") nil t 1)
               (replace-match "" nil nil nil 0))))
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

(defun gpt-commit--improve-message (msg callback)
  "Improve a commit message MSG using GPT and pass it to the CALLBACK."
  (let ((messages `[((role . "system")
                     (content . ,gpt-commit-improve-system-prompt))
                    ((role . "user")
                     (content . ,msg))]))
    (if (and gpt-commit-fallback-model
             (not (equal gpt-commit-fallback-model gpt-commit-model-name)))
        (gpt-commit-doc-gpt-request messages callback
                                    (lambda (error-thrown data)
                                      (message
                                       "GPT %s Error: %s %s, retrying with %s"
                                       gpt-commit-model-name
                                       error-thrown data
                                       gpt-commit-fallback-model)
                                      (gpt-commit-doc-gpt-request
                                       messages
                                       callback
                                       nil
                                       gpt-commit-fallback-model))
                                    gpt-commit-model-name)
      (gpt-commit-doc-gpt-request messages callback
                                  nil gpt-commit-model-name))))

(defun gpt-commit-get-commit-msg-end ()
  "Find the end position of the commit message in a buffer."
  (save-excursion
    (if (re-search-forward "^#" nil t 1)
        (progn (forward-char -1)
               (forward-line -1))
      (goto-char (point-max)))
    (while (looking-at "\n")
      (forward-line -1))
    (line-end-position)))

(defun gpt-commit-improve-message ()
  "Improve the current git commit message using GPT."
  (interactive)
  (let* ((buffer (current-buffer))
         (msg (git-commit-buffer-message))
         (regex (concat (gpt-commit-get-commit-type-regex)  "\\([(][^)]+[)]\\)?"
                        ":[\s\t]*"))
         (prefix
          (and msg
               (with-temp-buffer (insert msg)
                                 (goto-char (point-min))
                                 (when (re-search-forward regex nil
                                                          t 1)
                                   (match-string-no-properties 0))))))
    (unless msg
      (user-error "No commit message to improve"))
    (gpt-commit--improve-message
     (if prefix
         (substring-no-properties msg (length prefix))
       msg)
     (lambda (commit-message)
       (when commit-message
         (with-current-buffer buffer
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
             (insert "\n"))))))))

;;;###autoload
(defun gpt-commit-message ()
  "Automatically generate a conventional commit message using GPT-Commit.

This function is a hook intended to be added to `git-commit-setup-hook'.
When called, it analyzes the changes in the Git repository and generates
a conventional commit message using the GPT model.

The generated commit message follows the conventional commit format,
providing a structured description of the changes made in the commit.

To use this feature, make sure you have set the OpenAI API key and
GPT model name in the respective variables:
- `gpt-commit-openai-key'
- `gpt-commit-model-name'
- `gpt-commit-fallback-model'

Example usage.
  (require \\='gpt-commit)
  (setq gpt-commit-openai-key \"YOUR_OPENAI_API_KEY\")
  (setq gpt-commit-model-name \"gpt-4)
  (setq gpt-commit-fallback-model \"gpt-3.5-turbo-16k\")."
  (interactive)
  (let ((buffer (current-buffer))
        (msg (git-commit-buffer-message)))
    (unless msg
      (setq msg (gpt-commit-read-type))
      (insert msg (or (gpt-commit--retrieve-issue-key-from-branch) "") ":"))
    (gpt-commit-generate-message
     msg
     (lambda (commit-message)
       (when commit-message
         (with-current-buffer buffer
           (if (bobp)
               (insert commit-message)
             (replace-region-contents (point-min)
                                      (point)
                                      (lambda () commit-message)))))))))

;;;###autoload (autoload 'gpt-commit-menu "gpt-commit" nil t)
(transient-define-prefix gpt-commit-menu ()
  "Git Commit Mode Menu."
  ["Cycle"
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
    :inapt-if-not git-commit-buffer-message)]
  ["Issue Key"
   ("i" gpt-commit-update-issue-key
    :inapt-if-not gpt-commit--retrieve-issue-key-from-branch
    :description (lambda ()
                   (concat "Insert issue key ("
                           (or (gpt-commit--retrieve-issue-key-from-branch)
                               "None")
                           ")")))]
  ["Insert"
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
   ("v" "Save" git-commit-save-message)
   ("l" "Cancel" with-editor-cancel)
   ("m" "Commit" with-editor-finish)])



(provide 'gpt-commit)
;;; gpt-commit.el ends here