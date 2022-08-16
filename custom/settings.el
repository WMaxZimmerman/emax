(delete-selection-mode 1)

(setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
(setq shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
;; (setq shell-file-name "/bin/bash")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "PID" nil)
;; (setenv "SHELL" shell-file-name)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(global-set-key [f1] 'shell)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(electric-pair-mode)
;;(server-start)

;; (add-hook 'rectangle-mark-mode-on-hook (lambda () (progn
;;                                                     (local-set-key (kbd "C-c SPC") 'mc/edit-lines)
;;                                                     (message "Custom Shortcuts for rectangle On."))))

;; (add-hook 'rectangle-mark-mode-off-hook (lambda () (progn
;;                                                      (local-unset-key (kbd "C-c SPC") nil)
;;                                                      (message "Custom Shortcuts for rectangle Off."))))

(auto-save-visited-mode t)

(setq ediff-split-window-function 'split-window-horizontally)
