(require 'oauth2)
(require 'alert)
(require 'slack)

;; === Currently Does Not Work ===

;; === Setup ===
;; (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;; (setq slack-prefer-current-team t)
;; (setq slack-buffer-create-on-notify t)
;; (setq slack-request-timeout 3600)
;; (fzl-create-empty-file-if-no-exists "~/slackcreds.el")
;; (load "~/slackcreds")

;; ;; === Key Bindings ===
;; (define-key slack-info-mode-map (kbd "M-u") 'slack-room-update-messages)

;; (define-key slack-mode-map (kbd "M-c") 'slack-buffer-kill)
;; (define-key slack-mode-map (kbd "M-r a") 'slack-message-add-reaction)
;; (define-key slack-mode-map (kbd "M-r r") 'slack-message-remove-reaction)
;; (define-key slack-mode-map (kbd "M-r s") 'slack-message-show-reaction-users)
;; (define-key slack-mode-map (kbd "M-p l") 'slack-room-pins-list)
;; (define-key slack-mode-map (kbd "M-p a") 'slack-message-pins-add)
;; (define-key slack-mode-map (kbd "M-p r") 'slack-message-pins-remove)
;; (define-key slack-mode-map (kbd "M-m m") 'slack-message-write-another-buffer)
;; (define-key slack-mode-map (kbd "M-m e") 'slack-message-edit)
;; (define-key slack-mode-map (kbd "M-m d") 'slack-message-delete)
;; (define-key slack-mode-map (kbd "M-m n") 'slack-buffer-goto-next-message)
;; (define-key slack-mode-map (kbd "M-m p") 'slack-buffer-goto-prev-message)
;; (define-key slack-mode-map (kbd "M-u") 'slack-room-update-messages)
;; (define-key slack-mode-map (kbd "M-2") 'slack-message-embed-mention)
;; (define-key slack-mode-map (kbd "M-3") 'slack-message-embed-channel)

;; (define-key slack-edit-message-mode-map (kbd "M-m k") 'slack-message-cancel-edit)
;; (define-key slack-edit-message-mode-map (kbd "M-m s") 'slack-message-send-from-buffer)
;; (define-key slack-edit-message-mode-map (kbd "M-m 2") 'slack-message-embed-mention)
;; (define-key slack-edit-message-mode-map (kbd "M-m 3") 'slack-message-embed-channel)

(require 'alert)
(setq alert-default-style 'notifier)
