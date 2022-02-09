(require 'mermaid-mode)
(require 'ob-mermaid)

;; === Setup ===
(setq ob-mermaid-cli-path "~/.nvm/versions/node/v12.13.1/bin/mmdc")
(setq mermaid-output-format ".svg")


;; === Key Binding ===
(setq mermaid-mode-map
  (let ((map mermaid-mode-map))
    (define-key map (kbd "C-c C-c") nil)
    (define-key map (kbd "C-c C-f") nil)
    (define-key map (kbd "C-c C-b") nil)
    (define-key map (kbd "C-c C-r") nil)
    (define-key map (kbd "C-c C-o") nil)
    (define-key map (kbd "C-c C-d") nil)
    (define-key map (kbd "C-c C-d c") 'mermaid-compile)
    (define-key map (kbd "C-c C-d c") 'mermaid-compile)
    (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
    (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
    (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
    (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
    (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
    map))
