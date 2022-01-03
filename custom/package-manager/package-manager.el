(defun ensure-package-installed (package)
  "Assure package is installed, ask for installation if it’s not."
  (setq package-symbol (intern package))
  (straight-use-package package-symbol))

(defun ensure-packages-are-installed ()
  (interactive)
  (message "ensuring packages")
  (mapc 'ensure-package-installed
	(with-temp-buffer
	  (insert-file-contents "~/.emacs.d/custom/package-manager/packages.db")
	  (split-string (buffer-string) "\n" t))))

(defun check-if-any-packages-are-needed ()
  (interactive)
  (message "ensuring packages")
  (member nil
	  (mapcar 'ensure-package-installed-test
		  (with-temp-buffer
		    (insert-file-contents "~/.emacs.d/custom/package-manager/packages.db")
		    (split-string (buffer-string) "\n" t)))))

(defun ensure-package-installed-test (package)
  "Assure package is installed, ask for installation if it’s not."
  (setq package-symbol (intern package))
  (package-installed-p package-symbol))

(defun ensure-all-packages-are-installed ()
  (if 'check-if-any-packages-are-needed
      (ensure-packages-are-installed)))

(defun add-package ()
  (interactive)
  (setq package (read-string "Package:"))
  (setq buff (find-file "~/.emacs.d/custom/package-manager/packages.db"))
  (end-of-buffer)
  (insert (format "%s\n" package))
  (save-buffer)
  (kill-buffer buff)
  (unless (package-installed-p package)
       (straight-use-package (intern package))))
