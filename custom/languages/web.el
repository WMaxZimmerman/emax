;;; package --- web
;;; Commentary:
;;; Shared things for front-end development

;;; Code:

;; === Testing ===
(require 'jest-test-mode)

(add-hook 'typescript-mode-hook 'jest-test-mode)
(add-hook 'js-mode-hook 'jest-test-mode)
(add-hook 'typescript-tsx-mode-hook 'jest-test-mode)


;; === Indentation ===
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode)) ;; Only .tsx files, not .ts
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode)) ;; Add this line for .ts files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'") ("tsx" . "\\.ts[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.razor\\'" . web-mode))

;; Set default indentation for web-mode
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-script-padding 2)
(setq-default web-mode-style-padding 2)
(setq-default web-mode-attr-indent-offset 2)

;; TypeScript/JavaScript indentation
(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq json-reformat:indent-width 2)

(defun web-mode-setup-indentation ()
  "Complete setup for web-mode indentation"
  (when (eq major-mode 'web-mode)
    ;; Set all web-mode specific variables locally
    (setq-local web-mode-markup-indent-offset 2)
    (setq-local web-mode-css-indent-offset 2)
    (setq-local web-mode-code-indent-offset 2)
    (setq-local web-mode-script-padding 2)
    (setq-local web-mode-style-padding 2)
    (setq-local web-mode-attr-indent-offset 2)
    (setq-local web-mode-attr-value-indent-offset 2)
    (setq-local web-mode-block-padding 2)
    (setq-local web-mode-part-padding 2)
    
    ;; Override any global settings locally
    (setq-local c-basic-offset 2)
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil)
    (setq-local standard-indent 2)
    
    ;; For JavaScript/TypeScript within web-mode
    (setq-local js-indent-level 2)
    (setq-local typescript-indent-level 2)
    
    ;; Force tab behavior to use 2 spaces
    (setq-local tab-stop-list (number-sequence 2 120 2))))

(add-hook 'web-mode-hook 'web-mode-setup-indentation)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

;; Additional indentation settings for comprehensive coverage
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)

;; JSON mode specific settings
(with-eval-after-load 'json-mode
  (setq json-reformat:indent-width 2)
  (setq json-encoding-default-indentation "  "))

;; Ensure prettier uses 2 spaces if you're using it
(with-eval-after-load 'prettier-js
  (setq prettier-js-args '("--tab-width" "2" "--use-tabs" "false")))

(defun debug-web-mode-indentation ()
  "Debug current indentation settings in web-mode"
  (interactive)
  (message "Mode: %s" major-mode)
  (message "web-mode-code-indent-offset: %s" web-mode-code-indent-offset)
  (message "c-basic-offset: %s" c-basic-offset)
  (message "tab-width: %s" tab-width)
  (message "standard-indent: %s" standard-indent)
  (message "indent-tabs-mode: %s" indent-tabs-mode)
  (message "tab-stop-list: %s" (take 10 tab-stop-list)))

(defun fix-web-indentation ()
  "Manually fix indentation in current buffer"
  (interactive)
  (force-web-mode-indentation)
  (message "Web indentation reset to 2 spaces"))

;; Force tab key to indent with 2 spaces
(defun web-mode-indent-2-spaces ()
  "Indent current line or region properly with 2-space increments"
  (interactive)
  (if (use-region-p)
      ;; For regions: indent each line by 2 spaces
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (beginning-of-line)
            (unless (looking-at "^[ \t]*$") ; Skip empty lines
              (insert "  ")) ; Insert exactly 2 spaces
            (forward-line 1)
            (setq end (+ end 2))))) ; Adjust end position for inserted spaces
    ;; For single line: use web-mode's built-in indentation
    (web-mode-indent-line)))

(add-hook 'web-mode-hook 
          (lambda () 
            ;; Only set these bindings if we're actually in web-mode
            (when (eq major-mode 'web-mode)
              (local-set-key (kbd "TAB") 'indent-for-tab-command)
              (local-set-key (kbd "C-M-\\") 'web-mode-buffer-indent)
              (local-set-key (kbd "C-c d") 'debug-web-mode-indentation)
              (local-set-key (kbd "C-c f") 'fix-web-indentation))))


;; === Testing Keybinds ===
;; (define-key web-mode-map (kbd "C-c C-l t p") 'jest-test-run-at-point)
;; (define-key web-mode-map (kbd "C-c C-l t b") 'jest-test-run)
;; (define-key web-mode-map (kbd "C-c C-l t a") 'jest-test-run-all-tests)
(define-key lsp-command-map (kbd "t p") 'jest-test-run-at-point)
(define-key lsp-command-map (kbd "t b") 'jest-test-run)
(define-key lsp-command-map (kbd "t a") 'jest-test-run-all-tests)

(load "~/.emacs.d/custom/languages/typescript")

(provide 'web)

;;; web.el ends here
