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
(server-start)

(auto-save-visited-mode t)

(setq ediff-split-window-function 'split-window-horizontally)
