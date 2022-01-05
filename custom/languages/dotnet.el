;;; package --- dotnet
;;; Commentary:
;;; Dotnet specific configurations for LSP

;;; Code:


(define-key lsp-command-map (kbd "t p") 'lsp-csharp-run-test-at-point)
(define-key lsp-command-map (kbd "t b") 'lsp-csharp-run-all-tests-in-buffer)

(provide 'dotnet)

;;; dotnet.el ends here
