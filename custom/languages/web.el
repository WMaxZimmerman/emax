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

(provide 'angular)

;;; web.el ends here
