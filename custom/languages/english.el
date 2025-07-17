;;; package --- english
;;; Commentary:
;;; Configuration to help when writing text documents.

;;; Code:
(add-to-list 'exec-path "~/.emacs.d/tools/Hunspell/bin/")

(setq ispell-program-name "hunspell")
(setq flyspell-issue-message-flag nil)
(require 'ivy)
(require 'ispell)
(require 'flyspell)


;; === Adds a hook to enable Spell check mode ===
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; === Adds a hook to enable Programming Spell check mode ===
(add-hook 'csharp-mode-hook (lambda ()(flyspell-prog-mode)))
(add-hook 'js-mode-hook (lambda ()(flyspell-prog-mode)))

;; === Flyspell Shortcuts ===
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)


;; === Switching Spell Check Dictionary ===

(require 'flyspell-correct-ivy)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
(define-key flyspell-mode-map (kbd "C-.") nil)

(provide 'english)

;;; english.el ends here
