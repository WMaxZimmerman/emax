;; Configure lsp-mode, lsp-ui, and company-lsp
(require 'lsp-mode)
(require 'lsp-ui)
(add-to-list 'lsp-language-id-configuration '(org-mode . "org"))
(add-hook 'org-mode-hook #'lsp)
(setq lsp-ui-doc-enable t)

(defun org-start-language-server ()
  "Start the Org-mode language server."
  (let ((org-language-server-command "dotnet")
        (org-language-server-arguments '("run" "--no-build" "--no-restore"))
        (org-language-server-path "/c/bench/git/lsp-talk/org-lsp/OrgModeLanguageServer"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         (append (list org-language-server-command)
                                 org-language-server-arguments
                                 (list (expand-file-name org-language-server-path)))))
      :activation-fn (lsp-activate-on "org")
      :server-id 'org-ls))))

(org-start-language-server)
