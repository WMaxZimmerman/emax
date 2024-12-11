;;; package --- web
;;; Commentary:
;;; Shared things for front-end development

;;; Code:

;; === Testing ===
(require 'jest-test-mode)

(add-hook 'typescript-mode-hook 'jest-test-mode)
(add-hook 'js-mode-hook 'jest-test-mode)
(add-hook 'typescript-tsx-mode-hook 'jest-test-mode)

;; === Framework Specifics ===
(load "~/.emacs.d/custom/languages/angular")
(load "~/.emacs.d/custom/languages/react")
(load "~/.emacs.d/custom/languages/typescript")

;; === Testing Keybinds ===
;; (define-key web-mode-map (kbd "C-c C-l t p") 'jest-test-run-at-point)
;; (define-key web-mode-map (kbd "C-c C-l t b") 'jest-test-run)
;; (define-key web-mode-map (kbd "C-c C-l t a") 'jest-test-run-all-tests)
(define-key lsp-command-map (kbd "t p") 'jest-test-run-at-point)
(define-key lsp-command-map (kbd "t b") 'jest-test-run)
(define-key lsp-command-map (kbd "t a") 'jest-test-run-all-tests)

(provide 'web)

;;; web.el ends here
