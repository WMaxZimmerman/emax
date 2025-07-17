;;; package --- sql
;;; Commentary:
;;; Dotnet specific configurations for LSP

;;; Code:
(require 'sql)
(require 'sqlplus)
(require 'sql-indent)

;; ============================= LSP ===========================================
(add-hook 'sql-mode-hook 'lsp)
(setq sql-backend 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
      '(((driver . "postgresql")
         (dataSourceName . "host=localhost port=5432 user=postgres password=password dbname=avvdb sslmode=disable"))
        ((driver . "postgresql")
         (dataSourceName . "host=localhost port=5432 user=postgres password=password dbname=phyrexian_revokers sslmode=disable"))
        ((driver . "postgresql")
         (dataSourceName . "host=phyrexian-revokers-postgres.c1uqwimyphu3.us-east-1.rds.amazonaws.com port=5432 user=postgres password=iE4H6iejFV7VD9D4 dbname=phyrexian_revokers sslmode=disable"))
        ((driver . "mysql")
         (dataSourceName . "Server=localhost;Database=sammy;User Id=yyoncho;Password=hunter2;"))
        ((driver . "oracle")
         (dataSourceName . "Data Source=(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=glvdeveldb.vermeermfg.com)(PORT=1521))(LOAD_BALANCE=yes)(CONNECT_DATA=(SERVER=DEDICATED) (SERVICE_NAME=glvdevel.vermeermfg.com)));User Id=WEB_API;Password=zDG4M#EQfV_9QYt;"))))

(with-eval-after-load 'sql
  (setq sql-connection-alist
        '((pool-a
           (sql-product 'postgresql)
           (sql-server "127.0.0.1")
           (sql-user "postgres")
           (sql-password "secret_db_password")
           (sql-database "postgres")
           (sql-port 5432))
          (pool-a
           (sql-product 'oracle)
           (sql-server "glvdeveldb.vermeermfg.com")
           (sql-user "WEB_API")
           (sql-password "zDG4M#EQfV_9QYt")
           (sql-database "glvdevel.vermeermfg.com")
           (sql-port 1521)))))

;; ============================= ORG ===========================================
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;; ((sql-mode . ((sql-postgres-login-params 
;;   '((user :default "postgres")
;;     (database :default "avvdb")
;;     (server :default "localhost")
;;     (port :default 5432))))))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; Silence compiler warnings
(defvar sql-product)
(defvar sql-prompt-regexp)
(defvar sql-prompt-cont-regexp)

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(defun my-sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (when (eq sql-product 'postgres)
    ;; Allow symbol chars in database names in prompt.
    ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
    (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] ")
    ;; Ditto for continuation prompt: "^\\w*[-(][#>] "
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] "))

  ;; Deal with inline prompts in query output.
  ;; Runs after `sql-interactive-remove-continuation-prompt'.
  (add-hook 'comint-preoutput-filter-functions
            'my-sql-comint-preoutput-filter :append :local))

(defun my-sql-comint-preoutput-filter (output)
  "Filter prompts out of SQL query output.

Runs after `sql-interactive-remove-continuation-prompt' in
`comint-preoutput-filter-functions'."
  ;; If the entire output is simply the main prompt, return that.
  ;; (i.e. When simply typing RET at the sqli prompt.)
  (if (string-match (concat "\\`\\(" sql-prompt-regexp "\\)\\'") output)
      output
    ;; Otherwise filter all leading prompts from the output.
    ;; Store the buffer-local prompt patterns before changing buffers.
    (let ((main-prompt sql-prompt-regexp)
          (any-prompt comint-prompt-regexp) ;; see `sql-interactive-mode'
          (prefix-newline nil))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (when (looking-at main-prompt)
          (setq prefix-newline t))
        (while (looking-at any-prompt)
          (replace-match ""))
        ;; Prepend a newline to the output, if necessary.
        (when prefix-newline
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))
        ;; Return the filtered output.
        (buffer-substring-no-properties (point-min) (point-max))))))

(defadvice sql-send-string (before my-prefix-newline-to-sql-string)
  "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `my-sql-comint-preoutput-filter'.)"
  (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))
(ad-activate 'sql-send-string)
