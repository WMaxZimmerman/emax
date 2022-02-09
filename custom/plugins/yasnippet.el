(yas-global-mode 1)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))
(yas-reload-all)

(defun do-yas-expand ()
  "Yasnippet expand."
  (let ((yas-maybe-expand 'return-nil))
    (yas-expand)))

(add-hook 'prog-mode-hook #'yas-minor-mode)
