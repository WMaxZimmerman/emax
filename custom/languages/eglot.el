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

(setq eglot-keymap-prefix "C-c C-l")

;; ;; === Settings ===
;; (setq lsp-ui-peek-always-show t

;; ;; === Servers ===
;; (add-to-list 'eglot-server-programs
;;     `(python-mode . ("pyls" "-v" "--tcp" "--host"
;;         "localhost" "--port" :autoport)))
;;(define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
;;(define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
;;(define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
(add-to-list 'eglot-server-programs
             `(csharp-mode . ("~/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/OmniSharp.exe" "-lsp")))

;; === Keybindings ===
(define-key eglot-mode-map (kbd "C-c C-l s r") 'eglot-reconect)
(define-key eglot-mode-map (kbd "C-c C-l s q") 'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c C-l = =") 'eglot-format-buffer)
(define-key eglot-mode-map (kbd "C-c C-l = a") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c C-l f d") 'xref-find-definition)
(define-key eglot-mode-map (kbd "C-c C-l f r") 'xref-find-references)
(define-key eglot-mode-map (kbd "C-c C-l f i") 'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-c C-l f t") 'lsp-find-typeDefinition)
(define-key eglot-mode-map (kbd "C-c C-l f v") 'eglot-find-declaration)
(define-key eglot-mode-map (kbd "C-c C-l r r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c C-l r o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c C-l r i") 'eglot-code-action-inline)
(define-key eglot-mode-map (kbd "C-c C-l r e") 'eglot-code-action-extract)
(define-key eglot-mode-map (kbd "C-c C-l a a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c C-l a q") 'eglot-code-action-quickfix)
(define-key eglot-mode-map (kbd "C-c C-l a r") 'eglot-code-action-rewrite)
(define-key eglot-mode-map (kbd "C-c C-l h") 'eldoc)

;; === Unmapped LSP Mode Bindings ===
;; (define-key eglot-mode-map (kbd "s-l s s") 'Entry point for the server startup.)
;; (define-key eglot-mode-map (kbd "F a") 'lsp-workspace-folders-add)
;; (define-key eglot-mode-map (kbd "F r") 'lsp-workspace-folders-remove)
;; (define-key eglot-mode-map (kbd "F b") 'lsp-workspace-blacklist-remove)
;; (define-key eglot-mode-map (kbd "T l") 'lsp-toggle-trace-io)
;; (define-key eglot-mode-map (kbd "T L") 'Toggle client-server protocol logging.)
;; (define-key eglot-mode-map (kbd "T h") 'lsp-toggle-symbol-highlight)
;; (define-key eglot-mode-map (kbd "T S") 'lsp-ui-sideline-toggle-symbols-info)
;; (define-key eglot-mode-map (kbd "T d") 'lsp-ui-peek--toggle-file)
;; (define-key eglot-mode-map (kbd "T s") 'lsp-toggle-signature-auto-activate)
;; (define-key eglot-mode-map (kbd "T f") 'lsp-toggle-on-type-formatting)
;; (define-key eglot-mode-map (kbd "T T") 'Toggle global minor mode for synchronizing lsp-mode workspace folders and treemacs projects. (requires lsp-treemacs))
;; (define-key eglot-mode-map (kbd "g h") 'Show the incoming call hierarchy for the symbol at point. (requires lsp-treemacs))
;; (define-key eglot-mode-map (kbd "g a") 'Find all meaningful symbols that match pattern.)
;; (define-key eglot-mode-map (kbd "h h") 'lsp-signature-mode)
;; (define-key eglot-mode-map (kbd "h s") 'lsp-signature-activate)
;; (define-key eglot-mode-map (kbd "h g") 'Trigger display hover information popup and hide it on next typing.)
;; (define-key eglot-mode-map (kbd "a l") 'Click lsp lens using ‘avy’ package.)
;; (define-key eglot-mode-map (kbd "a h") 'Highlight symbol at point.)
;; (define-key eglot-mode-map (kbd "G g") 'lsp-ui-peek-find-definitions)
;; (define-key eglot-mode-map (kbd "G r") 'lsp-ui-peek-find-references)
;; (define-key eglot-mode-map (kbd "G i") 'lsp-ui-peek-find-implementation)
;; (define-key eglot-mode-map (kbd "G s") 'lsp-ui-peek-find-workspace-symbol)

(provide 'eglot)

;;; eglot.el ends here
