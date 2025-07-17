(delete-selection-mode 1)

;; Global indentation defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default standard-indent 2)

(if (eq system-type 'windows-nt)
    (progn
      (setq explicit-shell-file-name "C:\\bench\\tools\\scoop\\apps\\git\\current\\bin\\bash.exe")
      (setq shell-file-name "C:\\bench\\tools\\scoop\\apps\\git\\current\\bin\\bash.exe")
      (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
      (setenv "PID" nil)
      )
)

(global-set-key [f1] 'shell)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(electric-pair-mode)
(server-start)
(global-so-long-mode t)

(auto-save-visited-mode t)

(setq ediff-split-window-function 'split-window-horizontally)


;; EDiff Settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

;; ======== Org Shit =========
(define-key global-map (kbd "C-c e") 'org-table-recalculate-buffer-tables)

(add-hook 'ediff-mode-hook 'ora-ediff-hook)
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
