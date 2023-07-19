;;; package --- Other-GTD
;;; Commentary:
;;; For information on setup - https://github.com/rougier/emacs-gtd

(require 'org)
(require 'org-id)
(require 'org-ql)
(require 'dnd)

;; Functions
(defun org-my-toggle-agenda-file-set ()
  (interactive)
  (if (equal org-agenda-files "~/.emacs.d/agendas/.agenda_files_default")
      (progn (setq org-agenda-files "~/.emacs.d/agendas/.agenda_files_dnd")
             (setq org-directory "~/Dropbox/dnd"))
    (progn (setq org-agenda-files "~/.emacs.d/agendas/.agenda_files_default")
           (setq org-directory "~/Dropbox/gtd")))
  (message "Using %s" org-agenda-files))

(org-my-toggle-agenda-file-set)

;; Calendar
(require 'calendar)
(defun calendar-insert-date ()
  "Insert the date formatted date into buffer"
  (interactive)
    (insert (calendar-today-date-string)))

(defun calendar-today-date-string ()
  "Capture the date at point and return as formatted string"
  (setq mdy (calendar-current-date))
  (substring (format "[%02d-%02d-%02d]" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy))
             0))

;; Capture
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
        ("t" "tickler" entry  (file+headline "tickler.org" "Future")
        ,(concat "* %? :meeting:\n"
                 "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "notes.org")
        ,(concat "* Note (%a)\n"
                 "/Entered on/ %U\n" "\n" "%?"))
        ("w" "Weight" entry  (file+olp+datetree "health/weight.org" "Weight")
        ,(concat "* %? lbs\n" "\n")
        :tree-type month
        :kill-buffer t)
        ("f" "Food" entry  (file+olp+datetree "health/weight.org" "Food")
         ,(concat "\n* %?\n" "\n")
        :kill-buffer t)
        ("c" "Clock In" entry  (file+olp+datetree "work/timesheet.org")
         ,(concat "\n* start\n" "\n")
         :kill-buffer t
         :immediate-finish t
         :clock-in t
         :clock-keep t
         :clock-resume t
         :tree-type week)
        ("C" "Clock Out" entry  (clock)
         ,(concat "* stop" "\n")
         :kill-buffer t
         :immediate-finish t
         :clock-in t
         :clock-keep t
         :clock-resume t
         :tree-type week)
        ("a" "Abbreviation" entry  (file "abbreviations.org")
        ,(concat "* %? %^g\n"
                 "/Entered on/ %U\n"))
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
        ,(concat "* TODO Reply to \"%a\" %?\n"
                 "/Entered on/ %U"))))

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Key bindings
(define-key global-map            (kbd "C-c a") 'org-agenda)
(define-key global-map            (kbd "C-c c") 'org-capture)
(define-key global-map            (kbd "C-c i") 'org-capture-inbox)

;; Only if you use mu4e
;; (require 'mu4e)
;; (define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
;; (define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

;; TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; Agenda
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

;;; other-gtd.el ends here
