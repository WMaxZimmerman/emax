(menu-bar-mode -1)
(tool-bar-mode -1)

;; === Tabs Stuff ===
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; And I have tried
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; === Custom Things ===
(setq whitespace-line-column 900)
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
(load-theme 'nimbus t) ;; load material theme
(global-linum-mode 1)

(setq safe-local-variable-values
 (quote
  ((eval when
  (fboundp
   (quote rainbow-mode))
  (rainbow-mode 1)))))
(savehist-mode t)
(setq tab-stop-list
 (quote
  (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; ==== Custom Quote Block ===
(custom-set-faces
 '(org-fontify-quote-and-verse-blocks t)
 '(org-quote ((t (:inherit org-block :background "slate blue" :foreground "black")))))
