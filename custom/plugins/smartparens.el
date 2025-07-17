(require 'smartparens-config)

(add-hook 'js-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(show-paren-mode t)

(add-hook 'prog-mode-hook (lambda ()(highlight-parentheses-mode)))
