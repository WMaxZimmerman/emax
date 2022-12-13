(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'" . "tsx" . "\\.ts[x]?\\'")))
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")("tsx" . "\\.ts[x]?\\'")))
;; (setq web-mode-content-types-alist '(("tsx" . "\\.ts[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.razor\\'" . web-mode))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))
  
(add-hook 'web-mode-hook  'web-mode-init-hook)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-hook)

;;(add-hook 'web-mode-hook  'emmet-mode)
