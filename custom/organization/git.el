;;; package --- git
;;; Commentary:
;;; For information on setup - https://magit.vc/manual/forge/Getting-Started.html

(require 'magit)


(with-eval-after-load 'magit (require 'forge))

(push '("github.deere.com" "github.deere.com/api/v3"
        "github.deere.com" forge-github-repository)
        forge-alist)

(provide 'git)

;;; git.el ends here
