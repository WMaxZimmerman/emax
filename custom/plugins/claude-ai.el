;;; claude-ai.el --- Claude AI integration for Emacs

;;; Commentary:
;; This package provides integration with Claude AI using the Anthropic API.
;; You'll need to set your API key in `claude-ai-api-key`.

;;; Code:

(require 'json)
(require 'url)

(defgroup claude-ai nil
  "Claude AI integration for Emacs."
  :group 'tools
  :prefix "claude-ai-")

(defcustom claude-ai-api-key nil
  "Your Anthropic API key for Claude AI.
Get one from https://console.anthropic.com"
  :type 'string
  :group 'claude-ai)

(defcustom claude-ai-model "claude-sonnet-4-20250514"
  "Claude model to use."
  :type 'string
  :group 'claude-ai)

(defcustom claude-ai-max-tokens 4000
  "Maximum tokens for Claude responses."
  :type 'integer
  :group 'claude-ai)

(defcustom claude-ai-temperature 0.7
  "Temperature for Claude responses (0.0 to 1.0)."
  :type 'float
  :group 'claude-ai)

(defvar claude-ai-base-url "https://api.anthropic.com/v1/messages"
  "Base URL for Claude API.")

(defvar claude-ai-buffer-name "*Claude AI*"
  "Name of the buffer for Claude responses.")

(defun claude-ai--make-request (messages &optional callback)
  "Make a request to Claude AI with MESSAGES.
If CALLBACK is provided, make an asynchronous request."
  (unless claude-ai-api-key
    (error "Please set `claude-ai-api-key` with your Anthropic API key"))
  
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("x-api-key" . ,claude-ai-api-key)
           ("anthropic-version" . "2023-06-01")))
        (url-request-data
         (json-encode
          `((model . ,claude-ai-model)
            (max_tokens . ,claude-ai-max-tokens)
            (temperature . ,claude-ai-temperature)
            (messages . ,messages)))))
    
    (if callback
        (url-retrieve claude-ai-base-url callback)
      (with-current-buffer (url-retrieve-synchronously claude-ai-base-url)
        (claude-ai--parse-response)))))

(defun claude-ai--parse-response ()
  "Parse the HTTP response from Claude API."
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (let* ((json-response (buffer-substring (point) (point-max)))
           (parsed (json-read-from-string json-response)))
      (if (assoc 'error parsed)
          (error "Claude API error: %s" (cdr (assoc 'message (cdr (assoc 'error parsed)))))
        (let ((content (cdr (assoc 'content (aref (cdr (assoc 'content parsed)) 0)))))
          content)))))

(defun claude-ai--format-messages (user-message &optional system-message)
  "Format messages for Claude API."
  (let ((messages (list `((role . "user") (content . ,user-message)))))
    (when system-message
      (push `((role . "system") (content . ,system-message)) messages))
    (reverse messages)))

;;;###autoload
(defun claude-ai-ask (prompt)
  "Ask Claude AI a question with PROMPT."
  (interactive "sAsk Claude: ")
  (let* ((messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Q: %s\n\n" prompt))
      (insert (format "A: %s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-explain-region (start end)
  "Ask Claude to explain the selected region."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (prompt (format "Please explain this code:\n\n%s" text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Code:\n%s\n\n" text))
      (insert (format "Explanation:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-improve-code (start end)
  "Ask Claude to suggest improvements for the selected code."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (prompt (format "Please suggest improvements for this code:\n\n%s" text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Original code:\n%s\n\n" text))
      (insert (format "Suggestions:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-debug-code (start end)
  "Ask Claude to help debug the selected code."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (prompt (format "Please help me debug this code and find potential issues:\n\n%s" text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Code to debug:\n%s\n\n" text))
      (insert (format "Debug suggestions:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-generate-docstring (start end)
  "Generate documentation for the selected function."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (prompt (format "Please generate documentation/docstring for this function:\n\n%s" text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Function:\n%s\n\n" text))
      (insert (format "Documentation:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-complete-code ()
  "Complete code at point using Claude AI."
  (interactive)
  (let* ((context (claude-ai--get-context-around-point))
         (language (claude-ai--detect-language))
         (prompt (format "Complete this %s code. Only return the completion, no explanations:\n\n%s" 
                        language context))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (insert (string-trim response))))

;;;###autoload
(defun claude-ai-refactor-function ()
  "Refactor the current function using Claude AI."
  (interactive)
  (let* ((func-bounds (claude-ai--get-function-bounds))
         (func-text (buffer-substring-no-properties (car func-bounds) (cdr func-bounds)))
         (language (claude-ai--detect-language))
         (prompt (format "Refactor this %s function to improve readability and efficiency. Return only the refactored code:\n\n%s" 
                        language func-text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (when (yes-or-no-p "Replace current function with refactored version? ")
      (delete-region (car func-bounds) (cdr func-bounds))
      (insert (string-trim response)))))

;;;###autoload
(defun claude-ai-add-tests ()
  "Generate unit tests for the selected function."
  (interactive)
  (let* ((func-bounds (claude-ai--get-function-bounds))
         (func-text (buffer-substring-no-properties (car func-bounds) (cdr func-bounds)))
         (language (claude-ai--detect-language))
         (prompt (format "Generate comprehensive unit tests for this %s function:\n\n%s" 
                        language func-text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Function:\n%s\n\n" func-text))
      (insert (format "Generated Tests:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-explain-error ()
  "Explain the error at point or in compilation buffer."
  (interactive)
  (let* ((error-text (claude-ai--get-error-context))
         (language (claude-ai--detect-language))
         (prompt (format "Explain this %s error and suggest how to fix it:\n\n%s" 
                        language error-text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Error:\n%s\n\n" error-text))
      (insert (format "Explanation:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-optimize-code (start end)
  "Optimize the selected code for performance."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (language (claude-ai--detect-language))
         (prompt (format "Optimize this %s code for performance while maintaining functionality:\n\n%s" 
                        language text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (with-current-buffer (get-buffer-create claude-ai-buffer-name)
      (erase-buffer)
      (insert (format "Original code:\n%s\n\n" text))
      (insert (format "Optimized version:\n%s\n" response))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun claude-ai-generate-comments ()
  "Add inline comments to the selected code."
  (interactive)
  (let* ((func-bounds (claude-ai--get-function-bounds))
         (func-text (buffer-substring-no-properties (car func-bounds) (cdr func-bounds)))
         (language (claude-ai--detect-language))
         (prompt (format "Add helpful inline comments to this %s code. Return the complete code with comments:\n\n%s" 
                        language func-text))
         (messages (claude-ai--format-messages prompt))
         (response (claude-ai--make-request messages)))
    (when (yes-or-no-p "Replace current function with commented version? ")
      (delete-region (car func-bounds) (cdr func-bounds))
      (insert (string-trim response)))))

;; Helper functions for IDE features
(defun claude-ai--get-context-around-point (&optional lines)
  "Get context around point for code completion."
  (let ((lines (or lines 10)))
    (save-excursion
      (let ((start (progn (forward-line (- lines)) (point)))
            (end (progn (forward-line (* 2 lines)) (point))))
        (buffer-substring-no-properties start end)))))

(defun claude-ai--detect-language ()
  "Detect the programming language of the current buffer."
  (let ((mode (symbol-name major-mode)))
    (cond
     ((string-match "python" mode) "Python")
     ((string-match "javascript\\|js" mode) "JavaScript")
     ((string-match "typescript\\|ts" mode) "TypeScript")
     ((string-match "java" mode) "Java")
     ((string-match "c\\+\\+" mode) "C++")
     ((string-match "c-mode" mode) "C")
     ((string-match "go" mode) "Go")
     ((string-match "rust" mode) "Rust")
     ((string-match "php" mode) "PHP")
     ((string-match "ruby" mode) "Ruby")
     ((string-match "elisp\\|emacs-lisp" mode) "Emacs Lisp")
     ((string-match "lisp" mode) "Lisp")
     ((string-match "clojure" mode) "Clojure")
     ((string-match "haskell" mode) "Haskell")
     ((string-match "scala" mode) "Scala")
     ((string-match "kotlin" mode) "Kotlin")
     ((string-match "swift" mode) "Swift")
     ((string-match "shell\\|bash" mode) "Shell")
     (t "code"))))

(defun claude-ai--get-function-bounds ()
  "Get the bounds of the current function."
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (cons start end))))

(defun claude-ai--get-error-context ()
  "Get error context from compilation buffer or current line."
  (if (get-buffer "*compilation*")
      (with-current-buffer "*compilation*"
        (buffer-substring-no-properties (point-min) (point-max)))
    (thing-at-point 'line t)))

;; Key bindings
(defvar claude-ai-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Basic commands with C-c C-a prefix
    (define-key map (kbd "C-c C-a a") 'claude-ai-ask)
    (define-key map (kbd "C-c C-a e") 'claude-ai-explain-region)
    (define-key map (kbd "C-c C-a i") 'claude-ai-improve-code)
    (define-key map (kbd "C-c C-a d") 'claude-ai-debug-code)
    (define-key map (kbd "C-c C-a g") 'claude-ai-generate-docstring)
    
    ;; IDE-like features
    (define-key map (kbd "C-c C-a c") 'claude-ai-complete-code)
    (define-key map (kbd "C-c C-a r") 'claude-ai-refactor-function)
    (define-key map (kbd "C-c C-a t") 'claude-ai-add-tests)
    (define-key map (kbd "C-c C-a x") 'claude-ai-explain-error)
    (define-key map (kbd "C-c C-a o") 'claude-ai-optimize-code)
    (define-key map (kbd "C-c C-a m") 'claude-ai-generate-comments)
    map)
  "Keymap for Claude AI commands.")

;;;###autoload
(define-minor-mode claude-ai-mode
  "Minor mode for Claude AI integration."
  :lighter " Claude"
  :keymap claude-ai-mode-map
  :group 'claude-ai)

;;;###autoload
(defun claude-ai-setup ()
  "Setup Claude AI integration."
  (interactive)
  (unless claude-ai-api-key
    (setq claude-ai-api-key 
          (read-string "Enter your Anthropic API key: ")))
  (claude-ai-mode 1)
  (message "Claude AI integration enabled! Use C-c c [command]"))

(provide 'claude-ai)

;;; claude-ai.el ends here
