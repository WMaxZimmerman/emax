(require 'smartparens-config)

;; (smartparens-global-mode t)
(add-hook 'js-mode-hook #'smartparens-strict-mode)
;; (add-hook 'csharp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(show-paren-mode t)

;;(require 'highlight-parentheses)
(add-hook 'prog-mode-hook (lambda ()(highlight-parentheses-mode)))
