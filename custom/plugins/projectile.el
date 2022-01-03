(require 'projectile)

(projectile-mode +1)

;; === Configuration ===
(setq projectile-indexing-method 'alien)
(projectile-register-project-type 'csharp '("*.sln"))

;; === Keybinding ===
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

