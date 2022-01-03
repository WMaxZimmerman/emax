;;; package --- csharp
;;; Commentary:
;;; Configuration to help make working in C# easier.

;;; Code:
(require 'package)
(require 'omnisharp)
(require 'company)

(fset 'yes-or-no-p 'y-or-n-p) ;;stop asking me to type ‘yes’ as a confirmation
(setq omnisharp-server-executable-path "~/.emacs.d/tools/omnisharp-roslyn/OmniSharp.exe")

;; === Functions ===
(defun find-project-root ()
  "Find the root of the current project directory."
  (interactive)
  (if (ignore-errors (eproject-root))
      (eproject-root)
    (or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))

(defun find-git-repo (dir)
  "Find the git repository for DIR."
  (if (string= "/" dir) full
      nil
    (if (file-exists-p (expand-file-name "../.git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))


(defun file-path-to-namespace ()
  "Get the file path to the namespace."
  (interactive)
  (let (
        (root (find-project-root))
        (base (file-name-nondirectory buffer-file-name))
        )
    (substring (replace-regexp-in-string "/" "\." (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
    )
  )

;; === Tab Completion ===
(defun do-yas-expand ()
  "Yasnippet expand."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Auto-complete on tab."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(define-key company-active-map (kbd "<C-tab>") 'tab-indent-or-complete)
(defvar yas-snippet-dirs
      '("~/.emacs.d/yasnippet-csharp"))

;; === Company ===
(require 'company)
(setq company-begin-commands '(self-insert-command))
(setq omnisharp-company-do-template-completion t)
(setq company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
(setq company-idle-delay 0.03)
(setq company-minimum-prefix-length 1)
(setq company-require-match nil)
(setq company-show-numbers t)
(defvar helm-ag-insert-at-point (quote word))
(setq omnisharp-auto-complete-want-documentation nil)
(setq omnisharp-company-sort-results t)

(savehist-mode t)
(show-paren-mode t)

(defun company-complete-selection-insert-key(company-key)
  "Auto-complete via COMPANY-KEY."
  (company-complete-selection)
  (insert company-key))

(defun company-complete-selection-insert-key-and-complete(company-key)
  "Company complete selection via COMPANY-KEY."
  (company-complete-selection-insert-key company-key)
  (company-complete))

(defun csharp-indent-function-on-closing-brace()
  "Indent function when closing brace is typed."
  (interactive)
  (insert "}")
  (c-indent-defun))

;; === Neotree ===
;; find current buffer in directory
(global-set-key (kbd "C-M-l") 'neotree-find)
(global-set-key (kbd "<f7>") 'neotree-toggle)
;; Not sure what these do, but they are interesting
(global-set-key (kbd "C-M-<left>") 'er/expand-region)
(global-set-key (kbd "C-M-<right>") 'er/contract-region)

;; === Key-Chord ===
;; (require 'key-chord)
;; (key-chord-mode 1)

;; (setq key-chord-one-key-delay 0.2)
;; (setq key-chord-two-keys-delay 0.15)
;; (define-key global-map (kbd "C-,") 'helm-projectile)
;; (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
;; (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)

;; === highlight-parentheses ===
;; (require 'highlight-parentheses)
;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)
;; (setq ring-bell-function 'ignore)

;;(projectile-global-mode)
;;(setq projectile-indexing-method 'alien)

(setq c-basic-offset 4) ; indents 4 chars
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces

;; === Omnisharp ===
;; (setq omnisharp-server-executable-path "~\\.emacs.d\\tools\\omnisharp-roslyn\\OmniSharp.exe")

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))


(defun my-csharp-mode ()
  "Definition for my csharp mode."
  (add-to-list 'company-backends 'company-omnisharp)
  (yas-minor-mode)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (electric-pair-mode)
  (modify-syntax-entry ?_ "_")
  (setq c-basic-offset 4) ; indents 4 chars
  (setq tab-width 4)          ; and 4 char wide for TAB
  (setq indent-tabs-mode nil) ; And force use of spaces
  (turn-on-eldoc-mode))
  (setq eldoc-idle-delay 0.1
      flycheck-display-errors-delay 0.2)

(setq omnisharp-company-strip-trailing-brackets nil)
(add-hook 'csharp-mode-hook 'my-csharp-mode)

(defun omnisharp-unit-test (mode)
  "Run test after building the solution.  MODE should be one of 'single', 'fixture' or 'all'."
  (interactive)
  (let ((test-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "gettestcontext")
          (cons `("Type" . ,mode) (omnisharp--get-common-params)))))
    (let ((test-command
           (cdr (assoc 'TestCommand test-response)))

          (test-directory
           (cdr (assoc 'Directory test-response))))
      (cd test-directory)
      (compile test-command))))

(setq omnisharp-company-match-type 'company-match-flx)
(setq gc-cons-threshold 20000000)

(defun csharp-newline-and-indent ()
  "Open a newline and indent.
If point is between a pair of braces, opens newlines to put braces
on their own line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (and
             (looking-at " *}")
             (save-match-data
               (when (looking-back "{ *")
                 (goto-char (match-beginning 0))
                 (unless (looking-back "^[[:space:]]*")
                   (newline-and-indent))
                 t)))
        (unless (and (boundp electric-pair-open-newline-between-pairs)
                     electric-pair-open-newline-between-pairs
                     electric-pair-mode)
          (goto-char (match-beginning 0))
          (newline-and-indent)))))
  (newline-and-indent)) 

;; === Keybindings ===
(define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
(define-key company-active-map (kbd "]") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '"]")))
(define-key company-active-map (kbd "[") (lambda() (interactive) (company-complete-selection-insert-key '"[")))
(define-key company-active-map (kbd ")") (lambda() (interactive) (company-complete-selection-insert-key '")")))
(define-key company-active-map (kbd "<SPC>") nil)
(define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
(define-key company-active-map (kbd ">") (lambda() (interactive) (company-complete-selection-insert-key '">")))
(define-key omnisharp-mode-map (kbd "}") 'csharp-indent-function-on-closing-brace) 
(define-key omnisharp-mode-map (kbd "<RET>") 'csharp-newline-and-indent) 

(define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "M-p") 'omnisharp-navigate-up)
(define-key omnisharp-mode-map (kbd "M-n") 'omnisharp-navigate-down)
(define-key omnisharp-mode-map (kbd "M-<f7>") 'omnisharp-find-usages)

(define-key omnisharp-mode-map (kbd "M-i") 'omnisharp-helm-find-implementations)
(define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)

(define-key omnisharp-mode-map (kbd "C-k C-d") 'omnisharp-code-format)
(define-key omnisharp-mode-map (kbd "C-d") 'duplicate-current-line-or-region)

(define-key omnisharp-mode-map (kbd "<f2>") 'omnisharp-rename-interactively)
(define-key omnisharp-mode-map (kbd "<f5>") 'omnisharp-build-in-emacs)

;; disable emacs ctrl-r key.... we need it for VS shortcuts
(global-unset-key "\C-r")
(define-key omnisharp-mode-map (kbd "C-r C-l") 'recompile)
(define-key omnisharp-mode-map (kbd "C-r C-r") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "<C-.>") 'omnisharp-run-code-action-refactoring)

(global-set-key (kbd "C-c o") 'omnisharp-start-omnisharp-server)

;; === Things I Decided (Not From Prebuilt) ===


(provide 'csharp)

;;; csharp.el ends here
