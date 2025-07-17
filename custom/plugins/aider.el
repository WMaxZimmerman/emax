;;(setenv "ANTHROPIC_API_KEY" "sk-...")
(setq aidermacs-default-chat-mode 'architect)
(setq aidermacs-default-model "sonnet")
(setq aidermacs-auto-accept-architect t)

;; Enable file watching
(setq aidermacs-watch-files t)

;; Kill the Aider buffer when exiting the session
(setq aidermacs-exit-kills-buffer t)

(global-set-key (kbd "C-c C-a") 'aidermacs-transient-menu)
