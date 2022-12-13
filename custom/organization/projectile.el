(require 'projectile)
(setq projectile-git-submodule-command nil)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode)

(add-to-list 'projectile-globally-ignored-directories "/*/node_modules")
(add-to-list 'projectile-globally-ignored-directories "/*/.git")
;;(projectile-mode +1)
