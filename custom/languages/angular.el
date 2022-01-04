;;; package --- angular
;;; Commentary:
;;; Manual Language Server for angular.

;;; Code:
(setq lsp-clients-angular-language-server-command
  '("node"
    "node_modules/@angular/language-server"
    "--ngProbeLocations"
    "node_modules"
    "--tsProbeLocations"
    "node_modules"
    "--stdio"))

(provide 'angular)

;;; angular.el ends here
