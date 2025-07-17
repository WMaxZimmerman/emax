;;; package --- typescript
;;; Commentary:
;;; Settings for typescript

(require 'dap-node)
;;(require 'dap-chrome)
(unless (file-exists-p dap-node-debug-path) (dap-node-setup))
;;(dap-chrome-setup)

(if (file-exists-p (nth 1 dap-node-debug-program))
    (message "NodeJS debugger successfully installed")
  (message "NodeJS debugger install failed. Please download it manually"))

;; (if (file-exists-p (nth 1 dap-chrome-debug-program))
;;     (message "Chrome debugger successfully installed")
;;   (message "Chrome debugger install failed. Please download it manually"))

;; (dap-register-debug-template
;;   "Node Typscript"
;;   (list :type "node"
;;         :request "launch"
;;         :program "/c/bench/git/la-casita/test/src/index.ts"
;;         :outFiles ["/c/bench/git/la-casita/test/dist/**/*.js"]
;;         :name "Node Typescript"))

(dap-register-debug-template "Node Attach"
    (list :type "node"
          :request "attach"
          :port 9229
          :name "Node Attach"
          :sourceMaps t
          :program "${workspaceFolder}/src/index.ts"
          :skipFiles ["<node_internals>/**"]
          :cwd "${workspaceFolder}"
          :remoteRoot "${workspaceFolder}"
          :localRoot "${workspaceFolder}"
          :outFiles ["${workspaceFolder}/dist"]))


