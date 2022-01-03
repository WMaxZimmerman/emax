;;; package --- GTD
;;; Commentary:
;;; For information on setup - https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

;;; Code:
;; Agenda
(defvar org-agenda-files '("~/gtd/inbox.org"
                           "~/gtd/gtd.org"
                           "~/gtd/files/projects/projects.org"
                           "~/gtd/tickler.org"))
;; Capture Templates
(defvar org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%? \n %U")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* TODO %i%? \n %U")
                              ("l" "Link" entry
                               (file+headline "~/gtd/resources/links.org" "Links")
                               "* %i%? \n %U")
                              ("p" "Persona" entry
                               (file+headline "~/gtd/resources/personas.org" "Personas")
                               "* %i%?")
                              ("n" "Note" entry
                               (file+headline "~/gtd/resources/notes.org" "Notes")
                               "* %i%?")))

;; Refiles
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; Keywords
(defvar org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Agendas
(defvar org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; Shortcuts
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c s") 'org-tags-view)

(provide 'gtd)

;;; gtd.el ends here
