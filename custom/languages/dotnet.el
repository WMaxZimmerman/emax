;;; package --- dotnet
;;; Commentary:
;;; Dotnet specific configurations for LSP

;;; Code:
(require 'dap-netcore)


;; (push "csharp-ls" lsp-disabled-clients) ;; avoid non-working client
(define-key lsp-command-map (kbd "t p") 'lsp-csharp-run-test-at-point)
(define-key lsp-command-map (kbd "t b") 'lsp-csharp-run-all-tests-in-buffer)
;;(setq lsp-csharp-server-path "~/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/OmniSharp.exe")


;; === Blazor ===
(require 'web-mode)
;;(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.razor\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.razor\\'" . csharp-mode))


;; === DAP ===
(setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.2.3-992/netcoredbg-win64.zip")
(setq dap-netcore-install-dir "c:/bench/tools")

(add-hook 'lsp-mode-on-hook (lambda () (progn 
                                         (dap-register-debug-template "Dotnet WEB"
                                                                      (list :type "coreclr"
                                                                            :request "launch"
                                                                            :mode "launch"
                                                                            :name "NetCoreDbg::Launch"
                                                                            :justMyCode t
                                                                            :program (concat (lsp-workspace-root)
                                                                                             "/"
                                                                                             (replace-regexp-in-string "\.sln" ".WEB" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             "/bin/Debug/net6.0/"
                                                                                             (replace-regexp-in-string "\.sln" ".WEB" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             ".dll")))
                                         (dap-register-debug-template "Dotnet API"
                                                                      (list :type "coreclr"
                                                                            :request "launch"
                                                                            :mode "launch"
                                                                            :name "NetCoreDbg::Launch"
                                                                            :justMyCode t
                                                                            :program (concat (lsp-workspace-root)
                                                                                             "/"
                                                                                             (replace-regexp-in-string "\.sln" ".API" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             "/bin/Debug/net6.0/"
                                                                                             (replace-regexp-in-string "\.sln" ".API" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             ".dll")))
                                         (dap-register-debug-template "Dotnet UI"
                                                                      (list :type "coreclr"
                                                                            :request "launch"
                                                                            :mode "launch"
                                                                            :name "NetCoreDbg::Launch"
                                                                            :justMyCode t
                                                                            :program (concat (lsp-workspace-root)
                                                                                             "/"
                                                                                             (replace-regexp-in-string "\.sln" ".UI" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             "/bin/Debug/net6.0/"
                                                                                             (replace-regexp-in-string "\.sln" ".UI" (nth 0 (directory-files (lsp-workspace-root) nil "\\.sln")))
                                                                                             ".dll")))
                                         (dap-register-debug-template "AVV"
                                                                      (list :type "coreclr"
                                                                            :request "launch"
                                                                            :mode "launch"
                                                                            :name "NetCoreDbg::Launch"
                                                                            :program (concat (lsp-workspace-root) "/api/avvAPI/bin/Debug/net6.0/avvAPI.dll")
                                                                            :dap-compilation "dotnet build")))))


;; === Functions ===
(defun find-project-root ()
  "Find the root of the current project directory."
  (interactive)
  (if (ignore-errors (eproject-root))
      (eproject-root)
    (or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))

(defun find-git-repo (dir)
  "Find the git repository for DIR."
  (if (string= "/" dir) full
      nil
    (if (file-exists-p (expand-file-name "../.git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))


(defun file-path-to-namespace ()
  "Get the file path to the namespace."
  (interactive)
  (let (
        (root (find-project-root))
        (base (file-name-nondirectory buffer-file-name))
        )
    (substring (replace-regexp-in-string "/" "\." (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
    )
  )


(load "~/.emacs.d/custom/languages/dotnet-dap")

(provide 'dotnet)

;;; dotnet.el ends here
