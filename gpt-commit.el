;;; gpt-commit.el --- Commit messages with GPT in Emacs -*- lexical-binding: t; -*-

;; Author: Youngwook Kim <youngwook.kim@gmail.com>
;; URL: https://github.com/ywkim/gpt-commit
;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (magit "2.90") (request "0.3.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; (setq gpt-commit-openai-key "YOUR_OPENAI_API_KEY")
;; (setq gpt-commit-model-name "gpt-3.5-turbo-16k")
;; (add-hook 'git-commit-setup-hook 'gpt-commit-message)


;;; Code:

(require 'magit)
(require 'request)

(defcustom gpt-commit-model-name "gpt-4"
  "Model name to use for GPT chat completions."
  :group 'gpt-commit
  :type 'string)

(defcustom gpt-commit-fallback-model "gpt-3.5-turbo-16k"
  "Fallback model name if `gpt-commit-model-name' is failed."
  :group 'gpt-commit
  :type '(radio
          (string :tag "API key" "gpt-3.5-turbo-16k")
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

(defun gpt-commit-openai-chat-completions-api (messages callback &optional
                                                        error-callback model)
  "Call OpenAI's Chat Completions API with MESSAGES and CALLBACK."
  (let* ((headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (if (functionp
                                                       'gpt-commit-openai-key)
                                                      (funcall
                                                       gpt-commit-openai-key)
                                                    gpt-commit-openai-key)))))
         (json-string (json-serialize `((model . ,(or model
                                                      gpt-commit-model-name))
                                        (messages . ,messages))))
         (payload (encode-coding-string json-string 'utf-8)))
    (request gpt-commit-api-url
      :type "POST"
      :headers headers
      :data payload
      :parser 'json-read
      :timeout 10
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (funcall callback (gpt-commit-parse-response data))))
      :error
      (cl-function
       (lambda (&rest args &key data error-thrown &allow-other-keys)
         (if error-callback
             (funcall error-callback error-thrown data)
           (message "GPT Error: %s %s" error-thrown data)))))))


(defun gpt-commit-generate-message (msg callback)
  "Generate a commit message using GPT and pass it to the CALLBACK."
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
        (gpt-commit-openai-chat-completions-api messages callback
                                                (lambda (error-thrown data)
                                                  (message
                                                   "GPT %s Error: %s %s, retrying with %s"
                                                   gpt-commit-model-name
                                                   error-thrown data
                                                   gpt-commit-fallback-model)
                                                  (gpt-commit-openai-chat-completions-api
                                                   messages
                                                   callback
                                                   nil
                                                   gpt-commit-fallback-model))
                                                gpt-commit-model-name)
      (gpt-commit-openai-chat-completions-api messages callback
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
  (setq gpt-commit-fallback-model \"gpt-3.5-turbo-16k\") "
  (interactive)
  (let ((buffer (current-buffer))
        (msg (git-commit-buffer-message)))
    (unless msg
      (setq msg (gpt-commit-read-type))
      (insert msg))
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



(provide 'gpt-commit)
;;; gpt-commit.el ends here