;;; package --- java
;;; Commentary:
;;; Configuration to help make working in java easier.

;;; Code:
(require 'package)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'dap-mode)
(require 'lsp-java)
(require 'dap-java)

;; Support Lombok in our projects, among other things
(setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:C:/Users/mzimmerman/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar" "-Xbootclasspath/a:C:/Users/mzimmerman/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"))


(provide 'java)

;;; java.el ends here
