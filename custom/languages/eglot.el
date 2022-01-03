;;; package --- eglot
;;; Commentary:
;;; Configuration to help when writing text documents.

;;; Code:
(require 'eglot)


;; ;; === Hooks ===
;; (add-hook 'prog-mode-hook #'lsp)
;; (add-hook 'prog-mode-hook 'flycheck-mode)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'eglot-mode-hook 'company-mode)
(add-hook 'eglot-mode-hook 'flymake-mode)

;; ;; === Settings ===
;; (setq lsp-ui-peek-always-show t

;; ;; === Servers ===
;; (add-to-list 'eglot-server-programs
;;     `(python-mode . ("pyls" "-v" "--tcp" "--host"
;;         "localhost" "--port" :autoport)))

;; === Key Bindings ===
;; === Refactor ===
(define-key eglot-mode-map (kbd "C-c C-r r") 'eglot-rename)
(define-key eglot-mode-map (kbd "M-<return>") 'eglot-code-actions)

;; === Format ===
(define-key eglot-mode-map (kbd "C-c C-f b") 'eglot-format-buffer)

;; ;; === Naviagation ===
;; (define-key eglot-mode-map (kbd "C-c C-n i") 'lsp-goto-implementation)
;; (define-key eglot-mode-map (kbd "C-c C-n t") 'lsp-goto-type-definition)
(define-key eglot-mode-map (kbd "C-c C-n d") 'xref-find-definitions)
;; (define-key eglot-mode-map (kbd "C-c C-n r") 'lsp-find-references)
;; (define-key eglot-mode-map (kbd "C-c C-n p") 'helm-imenu)

;; ;; === Peek ===
;; (define-key eglot-mode-map (kbd "C-c C-? r") 'lsp-ui-peek-find-references)
;; (define-key eglot-mode-map (kbd "C-c C-? d") 'lsp-ui-peek-find-definitions)
;; (define-key eglot-mode-map (kbd "C-c C-? i") 'lsp-ui-peek-find-implementation)
;; (define-key eglot-mode-map (kbd "C-c C-? s") 'lsp-ui-peek-find-workspace-symbol)

;; ;; === Describe ===
;; (define-key eglot-mode-map (kbd "C-c C-d s") 'lsp-describe-sessiong)
;; (define-key eglot-mode-map (kbd "C-c C-d p") 'lsp-describe-thing-at-point)

;; === Server ===
(define-key eglot-mode-map (kbd "C-c C-s r") 'eglot-reconect)
(define-key eglot-mode-map (kbd "C-c C-s s") 'eglot-shutdown)

(provide 'eglot)

;;; eglot.el ends here
