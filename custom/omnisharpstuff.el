(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

;Recursively add site-lisp to the load path
;Make sure custom stuff goes to the front of the list
(let ((default-directory "~/.emacs.d/site-lisp"))
  (let ((old-path (copy-sequence load-path))
                (new-load-path nil))
        (normal-top-level-add-to-load-path '("."))
        (normal-top-level-add-subdirs-to-load-path)
        (dolist (var load-path)
          (unless (memql var old-path)
                (add-to-list 'new-load-path var)
                (setq load-path (append new-load-path old-path))))))



;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key company-mode-map [escape] 'company-abort)

(fset 'yes-or-no-p 'y-or-n-p) ;;stop asking me to type ‘yes’ as a confirmation
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))


;; (define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet-csharp"))

(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t)))
  nil
  )

(require 'omnisharp)
(setq compilation-ask-about-save nil)


(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;VS keys
(define-key global-map (kbd "s-<left>") 'beginning-of-line)
(define-key global-map (kbd "s-<right>") 'end-of-line)
(define-key global-map (kbd "s-<up>") 'scroll-down)
(define-key global-map (kbd "s-<down>") 'scroll-up)
(define-key global-map (kbd "s-f") 'toggle-frame-fullscreen)
(define-key global-map (kbd "S-M-<return>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "C-g") 'goto-line)
(define-key global-map (kbd "C-F") 'helm-projectile-ag)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-i") 'isearch-forward)
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(global-set-key [(control tab)] 'bury-buffer)
(global-set-key [(control shift tab)] 'unbury-buffer)
;; enable ctrl-s to wrap around seeing as we disabled ctrl-r
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))


;; better than vim-vinegar
(require 'dired)

(global-set-key [M-left] 'elscreen-previous)
(global-set-key [M-right] 'elscreen-next)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq inhibit-splash-screen t) ;;disable splash screen
(fset 'yes-or-no-p 'y-or-n-p) ;;stop asking me to type ‘yes’ as a confirmation
(show-paren-mode t)
;;Ido mode for file completion:
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
;;ido for better buffer management:
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
;;window movement
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<down>") 'windmove-down)

(global-set-key (kbd "M-<up>") 'windmove-up)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; (setq backup-directory-alist
;;           `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;           `((".*" ,temporary-file-directory t)))
;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(setq ring-bell-function 'ignore)

(projectile-global-mode)
(setq projectile-indexing-method 'alien)

;; Default indentation settings (can be overridden by specific modes)
(setq-default c-basic-offset 4) ; indents 4 chars for C-like languages
(setq-default tab-width 4)      ; and 4 char wide for TAB
(setq-default indent-tabs-mode nil) ; And force use of spaces

;; C# specific settings
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
(load-directory "~/.emacs.d/config")

;; (add-hook 'term-mode-hook 'evil-emacs-state)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-mode-hook 'my-term-hook)

(setq system-uses-terminfo nil)
