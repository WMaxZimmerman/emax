;;; package --- python	
;;; Commentary:	
;;; Configuration to make development in python easier.	

;;; Code:	

(elpy-enable)	

;; use flycheck not flymake with elpy	
(when (require 'flycheck nil t)	
  (defvar elpy-modules (delq 'elpy-module-flymake elpy-modules))	
  (add-hook 'elpy-mode-hook 'flycheck-mode))	

;; enable autopep8 formatting on save	
(require 'py-autopep8)	
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)	

(defvar python-shell-interpreter "jupyter")	
(defvar python-shell-interpreter-args "console --simple-prompt")	
(defvar python-shell-prompt-detect-failure-warning nil)	

(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")	

(provide 'python)	

;;; python.el ends here
