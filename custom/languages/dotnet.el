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
(setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/3.1.0-1031/netcoredbg-win64.zip")
(setq dap-netcore-install-dir (expand-file-name "~/.emacs.d/debug-adapters/netcoredbg/"))

(with-eval-after-load 'dap-mode
  (dap-register-debug-template ".NET Core API Launch"
                               (list :type "coreclr"
                                     :request "launch"
                                     :mode "launch"
                                     :name "NetCoreDbg::API Launch"
                                     :justMyCode t
                                     :program "${workspaceFolder}/bin/Debug/net6.0/YourProjectName.dll"
                                     :cwd "${workspaceFolder}"
                                     :stopAtEntry nil
                                     :console "internalConsole")))


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

(defun dotnet-get-target-framework (root project-name)
  "Get the target framework for PROJECT-NAME in ROOT directory."
  (when (and root project-name)
    (let ((csproj-file (concat root "/" project-name "/" project-name ".csproj")))
      (when (file-exists-p csproj-file)
        (with-temp-buffer
          (insert-file-contents csproj-file)
          (goto-char (point-min))
          (if (re-search-forward "<TargetFramework>\\([^<]+\\)</TargetFramework>" nil t)
              (match-string 1)
            ;; Fallback: try TargetFrameworks (plural) and take the first one
            (when (re-search-forward "<TargetFrameworks>\\([^<;]+\\)" nil t)
              (match-string 1))))))))

(defun dotnet-find-executable-dll ()
  "Dynamically find the executable DLL for the current .NET project."
  (let* ((root (or (lsp-workspace-root) (find-project-root)))
         (sln-file (car (directory-files root nil "\\.sln$")))
         (base-name (when sln-file (replace-regexp-in-string "\\.sln$" "" sln-file))))
    (if base-name
        ;; Try different project suffixes in order of preference
        (let ((suffixes '(".Web" ".API" ".Api" ".UI" "")))
          (catch 'found
            (dolist (suffix suffixes)
              ;; Try both as-is and lowercase versions
              (dolist (case-suffix (list suffix (downcase suffix) (upcase suffix)))
                (let* ((project-name (concat base-name case-suffix))
                       (target-framework (dotnet-get-target-framework root project-name)))
                  (when target-framework
                    (let ((dll-path (concat root "/" project-name "/bin/Debug/" target-framework "/" project-name ".dll")))
                      (when (file-exists-p dll-path)
                        (throw 'found dll-path)))))))
            ;; If no built DLL found, try to find any .csproj and suggest building
            (let ((csproj-files (directory-files-recursively root "\\.csproj$")))
              (if csproj-files
                  (progn
                    (message "No built DLL found. Please run 'dotnet build' first.")
                    (car csproj-files)) ; Return first csproj as fallback
                (error "No .NET project found in %s" root)))))
      (error "No solution file found in %s" root))))

(defun dotnet-create-debug-template ()
  "Create a debug template for the current .NET project."
  (interactive)
  (let* ((dll-path (dotnet-find-executable-dll))
         (project-root (lsp-workspace-root))
         (template-name (format "Debug %s" (file-name-base dll-path))))
    (when dll-path
      (dap-debug (list :type "coreclr"
                       :request "launch"
                       :mode "launch"
                       :name template-name
                       :program dll-path
                       :cwd project-root
                       :stopAtEntry nil
                       :console "internalConsole"
                       :justMyCode t)))))

(defun dotnet-build-and-debug ()
  "Build the current .NET project and start debugging."
  (interactive)
  (let* ((root (or (lsp-workspace-root) (find-project-root)))
         (default-directory root))
    (message "Building .NET project...")
    (shell-command "dotnet build")
    (dotnet-create-debug-template)))

(defun dotnet-prompt-for-dll (root)
  "Prompt user to select a DLL file when automatic detection fails."
  (let ((dll-files (directory-files-recursively root "\\.dll$")))
    (if dll-files
        (completing-read "Select DLL to debug: " dll-files nil t)
      (error "No DLL files found in project. Please build your project first with 'dotnet build'"))))

(defun dotnet-install-debugger ()
  "Install the .NET Core debugger if not already installed."
  (interactive)
  (unless (file-exists-p (concat dap-netcore-install-dir "netcoredbg.exe"))
    (message "Installing .NET Core debugger...")
    (dap-netcore--debugger-install)))

(defun dotnet-setup-indentation ()
  "Setup 4-space indentation for C# files"
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local standard-indent 4))

(add-hook 'csharp-mode-hook 'dotnet-setup-indentation)

;; Add key bindings for debugging
(with-eval-after-load 'csharp-mode
  (define-key csharp-mode-map (kbd "C-c d d") 'dotnet-create-debug-template)
  (define-key csharp-mode-map (kbd "C-c d b") 'dotnet-build-and-debug))

;; Ensure debugger is available
(with-eval-after-load 'dap-mode
  (require 'dap-netcore)
  (dotnet-install-debugger))

(load "~/.emacs.d/custom/languages/dotnet-dap")

(provide 'dotnet)

;;; dotnet.el ends here
