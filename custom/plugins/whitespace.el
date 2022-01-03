(require 'whitespace)
(defvar whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'whitespace-mode)
