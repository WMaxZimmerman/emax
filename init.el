;; === SETUP ===
(setq lsp-keymap-prefix "C-c C-l")
(load "~/.emacs.d/straight/bootstrap")
(straight-use-package '(org :type built-in))

;; === Add Packages ===
(load "~/.emacs.d/custom/package-manager/package-manager")
(ensure-packages-are-installed)

;; === Compile Scripts ===
(defun check-for-compiled-file (path)
  "Check to see if a compiled file exists at PATH."
  (file-exists-p (concat path ".elc")))

(defun compile-if-no-compiled-file (path)
  "Check if a compiled file exists at PATH and create one if not or it is old."
  (if (check-for-compiled-file path)
      (if (file-newer-than-file-p (concat path ".el") (concat path ".elc"))
        (byte-compile-file (concat path ".el")))
      (byte-compile-file (concat path ".el"))))

(defun ensure-scripts-are-compiled()
  "Ensures that all scripts are compiled."
  (mapc 'compile-if-no-compiled-file (list
	 "~/.emacs.d/custom/package-manager/package-manager"
	 "~/.emacs.d/custom/plugins/projectile"
	 "~/.emacs.d/custom/plugins/ivy_settings"
	 "~/.emacs.d/custom/appearance/facelift"
	 "~/.emacs.d/custom/plugins/smartparens"
	 "~/.emacs.d/custom/plugins/whitespace"
;;	 "~/.emacs.d/custom/plugins/power-mode"
	 "~/.emacs.d/custom/plugins/mermaid"
	 "~/.emacs.d/custom/organization/backupfiles"
	 "~/.emacs.d/custom/organization/other-gtd"
	 "~/.emacs.d/custom/organization/jira"
	 "~/.emacs.d/custom/organization/toc"
	 "~/.emacs.d/custom/keyboard/shortcuts"
	 "~/Dropbox/dnd-mode/dnd-mode"
	 "~/.emacs.d/custom/plugins/dnd"
	 "~/.emacs.d/custom/mine/misc"
	 "~/.emacs.d/custom/settings"
	 "~/.emacs.d/custom/functions"
	 "~/.emacs.d/custom/languages/english"
	 "~/.emacs.d/custom/plugins/writegood-mode"
	 "~/.emacs.d/custom/plugins/slack"
	 "~/.emacs.d/custom/plugins/yasnippet"
	 "~/.emacs.d/custom/organization/ox-reveal"
     ;; === Programming Languages ===
	 "~/.emacs.d/custom/languages/lsp"
	 "~/.emacs.d/custom/languages/dotnet"
	 "~/.emacs.d/custom/languages/sql"
	 "~/.emacs.d/custom/languages/java"
	 "~/.emacs.d/custom/languages/angular"
	 ;; "~/.emacs.d/custom/languages/csharp"
	 ;; "~/.emacs.d/custom/languages/typescript"
	 ;; "~/.emacs.d/custom/languages/react"
	 ;; "~/.emacs.d/custom/languages/python"
	 "~/.emacs.d/custom/languages/yaml"
     ;; === Org Babel ===
	 "~/.emacs.d/custom/organization/org-babel/ob-csharp"
	 "~/.emacs.d/custom/organization/org-babel/ob-powershell"
	 "~/.emacs.d/custom/organization/org-babel/org-babel-settings"
	 "~/.emacs.d/custom/organization/plantuml-helpers")))
(ensure-scripts-are-compiled)

;; === Custom Scripts ===
(load "~/.emacs.d/custom/organization/backupfiles")
(load "~/.emacs.d/custom/organization/other-gtd")
(load "~/.emacs.d/custom/keyboard/shortcuts")
(load "~/.emacs.d/custom/functions")
(load "~/.emacs.d/custom/settings")
(load "~/Dropbox/dnd-mode/dnd-mode")
(load "~/.emacs.d/custom/plugins/dnd")
(load "~/.emacs.d/custom/mine/misc")

;; === Navigation/Searching ===
(load "~/.emacs.d/custom/plugins/projectile")
(load "~/.emacs.d/custom/plugins/dired+")
(load "~/.emacs.d/custom/plugins/ivy_settings")

;; === Appearance ===
(load "~/.emacs.d/custom/appearance/facelift")
;;(load "~/.emacs.d/custom/plugins/power-mode")

;; === Utility ===
(load "~/.emacs.d/custom/plugins/smartparens")
(load "~/.emacs.d/custom/plugins/whitespace")
(load "~/.emacs.d/custom/plugins/slack")
(load "~/.emacs.d/custom/plugins/yasnippet")
(load "~/.emacs.d/custom/plugins/mermaid")
(load "~/.emacs.d/custom/organization/jira")
(load "~/.emacs.d/custom/organization/toc")

;; === Org Babel ===
(load "~/.emacs.d/custom/organization/org-babel/ob-csharp")
(load "~/.emacs.d/custom/organization/org-babel/ob-powershell")
(load "~/.emacs.d/custom/organization/org-babel/org-babel-settings")

;; === Writing ===
(load "~/.emacs.d/custom/languages/english")
(load "~/.emacs.d/custom/plugins/writegood-mode")
(load "~/.emacs.d/custom/organization/ox-reveal")

;; === Programming ===
(load "~/.emacs.d/custom/languages/lsp")
(load "~/.emacs.d/custom/languages/sql")
;; (load "~/.emacs.d/custom/languages/csharp")
;; (load "~/.emacs.d/custom/languages/typescript")
;; (load "~/.emacs.d/custom/languages/python")
;; (load "~/.emacs.d/custom/languages/yaml")

;; === Tools ===
(load "~/.emacs.d/custom/organization/plantuml-helpers")

;; === Things Set Via Customize ===
;; === Move To Files and Delete ===
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fsharp-mode latex-preview-pane lsp-sh 0blayout flycheck-plantuml plantuml-mode ox-reveal htmlize flyspell-correct-ivy flyspell-correct nimbus-theme org-d20 multiple-cursors groovy-mode prettier-js add-node-modules-path web-mode tide sly slime-company key-chord py-autopep8 material-theme ein elpy omnisharp magit smartparens yasnippet-snippets yasnippet highlight-parentheses auto-complete projectile better-defaults neotree company counsel ivy use-package)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
