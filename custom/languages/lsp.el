;;; package --- lsp
;;; Commentary:
;;; Provides IDE features for multiple language


(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-ivy)
(require 'company-lsp)
(require 'dap-mode)
(require 'dap-ui)
(require 'dap-mouse)
(require 'dap-netcore)
(require 'flycheck)
(require 'lsp-java)
(require 'yasnippet)
(require 'company)

;; === Prefix ===
(setq lsp-keymap-prefix "C-c C-l")

;; === Performance ===
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-keep-workspace-alive nil)

;;(add-hook 'prog-mode-hook #'lsp)
(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode 'snippet-mode)
    (lsp-deferred)))

(add-hook 'prog-mode-hook #'dotfiles--lsp-deferred-if-supported)

;; === SQL ===
;; (setq lsp-sqls-connections
;;     '(((driver . "oracle") (connectionString . "<PutStuffHere>"))
;;       ((driver . "oracle") (dataSourceName . "<PutStuffHere>"))))



;; === YAS ===
(defun tab-indent-or-complete ()
  "Auto-complete on tab."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(define-key company-active-map (kbd "<C-tab>") 'tab-indent-or-complete)


;; === DAP ===
(dap-mode 1)
(dap-ui-mode 1)
(dap-auto-configure-mode)

(defun stop-debugging-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode 0)
  (dap-ui-mode 0)
  (dap-ui-controls-mode 0)
  (delete-other-windows) ;; hide all the dap UI. I might want to delete the buffers as well.
  )

(defun debugging-mode ()
  (interactive)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode)
  (dap-ui-controls-mode 1)
  (dap-ui-sessions)
  (dap-ui-locals)
  (dap-ui-breakpoints)
  (dap-ui-repl))


;; (dap-register-debug-template "Custome Dotnet"
;;                              (list :type "coreclr"
;;                                    :request "launch"
;;                                    :mode "launch"
;;                                    :name "NetCoreDbg::Launch"
;;                                    :justMyCode t))


;; === Keybindings ===
;; (define-key lsp-command-map (kbd "s-l s s") 'Entry point for the server startup.)
(define-key lsp-command-map (kbd "s r") 'lsp-workspace-restart)
(define-key lsp-command-map (kbd "s q") 'lsp-workspace-shutdown)
(define-key lsp-command-map (kbd "s d") 'lsp-describe-session)
(define-key lsp-command-map (kbd "s D") 'lsp-disconnect)
(define-key lsp-command-map (kbd "= =") 'lsp-format-buffer)
(define-key lsp-command-map (kbd "= r") 'lsp-format-region)
(define-key lsp-command-map (kbd "F a") 'lsp-workspace-folders-add)
(define-key lsp-command-map (kbd "F r") 'lsp-workspace-folders-remove)
(define-key lsp-command-map (kbd "F b") 'lsp-workspace-blacklist-remove)
(define-key lsp-command-map (kbd "T l") 'lsp-toggle-trace-io)
;; (define-key lsp-command-map (kbd "T L") 'Toggle client-server protocol logging.)
(define-key lsp-command-map (kbd "T h") 'lsp-toggle-symbol-highlight)
(define-key lsp-command-map (kbd "T S") 'lsp-ui-sideline-toggle-symbols-info)
(define-key lsp-command-map (kbd "T d") 'lsp-ui-peek--toggle-file)
(define-key lsp-command-map (kbd "T s") 'lsp-toggle-signature-auto-activate)
(define-key lsp-command-map (kbd "T f") 'lsp-toggle-on-type-formatting)
;; (define-key lsp-command-map (kbd "T T") 'Toggle global minor mode for synchronizing lsp-mode workspace folders and treemacs projects. (requires lsp-treemacs))
(define-key lsp-command-map (kbd "g g") 'lsp-find-definition)
(define-key lsp-command-map (kbd "g r") 'lsp-find-references)
(define-key lsp-command-map (kbd "g i") 'lsp-find-implementation)
(define-key lsp-command-map (kbd "g t") 'lsp-find-type-definition)
(define-key lsp-command-map (kbd "g d") 'lsp-find-declaration)
;; (define-key lsp-command-map (kbd "g h") 'Show the incoming call hierarchy for the symbol at point. (requires lsp-treemacs))
;; (define-key lsp-command-map (kbd "g a") 'Find all meaningful symbols that match pattern.)
(define-key lsp-command-map (kbd "h h") 'lsp-signature-mode)
(define-key lsp-command-map (kbd "h s") 'lsp-signature-activate)
;; (define-key lsp-command-map (kbd "h g") 'Trigger display hover information popup and hide it on next typing.)
(define-key lsp-command-map (kbd "r r") 'lsp-rename)
(define-key lsp-command-map (kbd "r o") 'lsp-organize-imports)
(define-key lsp-command-map (kbd "a a") 'lsp-execute-code-action)
;; (define-key lsp-command-map (kbd "a l") 'Click lsp lens using ‘avy’ package.)
;; (define-key lsp-command-map (kbd "a h") 'Highlight symbol at point.)
(define-key lsp-command-map (kbd "G g") 'lsp-ui-peek-find-definitions)
(define-key lsp-command-map (kbd "G r") 'lsp-ui-peek-find-references)
(define-key lsp-command-map (kbd "G i") 'lsp-ui-peek-find-implementation)
(define-key lsp-command-map (kbd "G s") 'lsp-ui-peek-find-workspace-symbol)


;; ==== DAP Keybindings ===

;; ==== Breakpoints ===
(define-key lsp-command-map (kbd "b a") 'dap-breakpoint-add)
(define-key lsp-command-map (kbd "b t") 'dap-breakpoint-toggle)
(define-key lsp-command-map (kbd "b c") 'dap-breakpoint-condition)

;; ==== Debugging ===
(define-key lsp-command-map (kbd "d s") 'debugging-mode)
(define-key lsp-command-map (kbd "d q") 'stop-debugging-mode)
(define-key lsp-command-map (kbd "d e") 'dap-eval)
(define-key lsp-command-map (kbd "d i") 'dap-step-in)
(define-key lsp-command-map (kbd "d o") 'dap-step-out)
(define-key lsp-command-map (kbd "d n") 'dap-next)
(define-key lsp-command-map (kbd "d c") 'dap-continue)

;; === Language Specifics ===
(load "~/.emacs.d/custom/languages/dotnet")
(load "~/.emacs.d/custom/languages/angular")
(load "~/.emacs.d/custom/languages/java")


(provide 'lsp)

;;; lsp.el ends here
