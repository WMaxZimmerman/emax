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
(let ((lombok-jar "C:/Users/mzimmerman/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"))
  (when (file-exists-p lombok-jar)
    (setq lsp-java-vmargs 
          `("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" 
            ,(concat "-javaagent:" lombok-jar)
            ,(concat "-Xbootclasspath/a:" lombok-jar)))))


(provide 'java)

;;; java.el ends here
