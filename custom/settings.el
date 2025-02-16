(delete-selection-mode 1)

(if (eq system-type 'windows-nt)
    (progn
      ;;(setq explicit-shell-file-name "C:\\bench\\tools\\scoop\\apps\\git\\current\\bin\\bash.exe")
      ;;(setq shell-file-name "C:\\bench\\tools\\scoop\\apps\\git\\current\\bin\\bash.exe")
      ;;(setq shell-file-name "bash")
      ;;(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
      ;;(setenv "PID" nil)
      (setq explicit-shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
      (setq shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe")
      (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
      (setenv "PID" nil)
      )
)

;;(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(global-set-key [f1] 'shell)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(electric-pair-mode)
(server-start)
(global-so-long-mode t)

;; (add-hook 'rectangle-mark-mode-on-hook (lambda () (progn
;;                                                     (local-set-key (kbd "C-c SPC") 'mc/edit-lines)
;;                                                     (message "Custom Shortcuts for rectangle On."))))

;; (add-hook 'rectangle-mark-mode-off-hook (lambda () (progn
;;                                                      (local-unset-key (kbd "C-c SPC") nil)
;;                                                      (message "Custom Shortcuts for rectangle Off."))))

(auto-save-visited-mode t)

(setq ediff-split-window-function 'split-window-horizontally)


;; EDiff Settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; (csetq ediff-diff-options "-w") ignores whitespace
(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

;; ======== Org Shit =========
(define-key global-map (kbd "C-c e") 'org-table-recalculate-buffer-tables)

(add-hook 'ediff-mode-hook 'ora-ediff-hook)
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
