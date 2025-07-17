(require 'restclient)
(require 'ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (clojure . t)
   (csharp . t)
   (ditaa . t)
   (dot . t)
   (plantuml . t)
   (powershell . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (shell . t)
   (restclient . t)))

(setq org-babel-restclient--jq-path "/c/bench/tools/scoop/shims/jq.exe")
