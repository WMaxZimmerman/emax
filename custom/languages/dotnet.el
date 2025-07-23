;;; package --- dotnet
;;; Commentary:
;;; Dotnet specific configurations for LSP

;;; Code:
(require 'dap-netcore)


(define-key lsp-command-map (kbd "t p") 'lsp-csharp-run-test-at-point)
(define-key lsp-command-map (kbd "t b") 'lsp-csharp-run-all-tests-in-buffer)


;; === Blazor ===
(require 'web-mode)
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
(defvar dotnet--project-root-cache nil
  "Cache for project root to avoid repeated filesystem calls.")

(defun find-project-root ()
  "Find the root of the current project directory."
  (interactive)
  (or dotnet--project-root-cache
      (setq dotnet--project-root-cache
            (or (when (fboundp 'eproject-root) (eproject-root))
                (find-git-repo (or buffer-file-name default-directory))
                (file-name-directory (or buffer-file-name default-directory))))))

(defun find-git-repo (dir)
  "Find the git repository for DIR."
  (when dir
    (let ((dir (expand-file-name dir)))
      (locate-dominating-file dir ".git"))))


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

(defun dotnet-setup-indentation ()
  "Setup 4-space indentation for C# files"
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local standard-indent 4))

(add-hook 'csharp-mode-hook 'dotnet-setup-indentation)

(provide 'dotnet)

;;; dotnet.el ends here
