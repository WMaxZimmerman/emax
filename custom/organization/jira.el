;;; package --- jira
;;; Commentary:
;;; For information on setup - https://github.com/ahungry/org-jira

(require 'org-jira)

(make-directory "~/.org-jira" :parents)
(setq jiralib-url "https://vermeercorp.atlassian.net")

;; === Keybindings ===
;; (define-key org-jira-map (kbd "C-c c c") 'org-jira-add-comment)
;; (define-key org-jira-map (kbd "C-c c u") 'org-jira-update-comment)

;; (define-key org-jira-map (kbd "C-c i R") 'org-jira-refresh-issues-in-buffer)
;; (define-key org-jira-map (kbd "C-c i a") 'org-jira-assign-issue)
;; (define-key org-jira-map (kbd "C-c i b") 'org-jira-browse-issue)
;; (define-key org-jira-map (kbd "C-c i c") 'org-jira-create-issue)
;; (define-key org-jira-map (kbd "C-c i f") 'org-jira-get-issues-by-fixversion)
;; (define-key org-jira-map (kbd "C-c i g") 'org-jira-get-issues)
;; (define-key org-jira-map (kbd "C-c i h") 'org-jira-get-issues-headonly)
;; (define-key org-jira-map (kbd "C-c i j") 'org-jira-get-issues-from-custom-jql)
;; (define-key org-jira-map (kbd "C-c i k") 'org-jira-copy-current-issue-key)
;; (define-key org-jira-map (kbd "C-c i n") 'org-jira-progress-issue-next)
;; (define-key org-jira-map (kbd "C-c i r") 'org-jira-refresh-issue)
;; (define-key org-jira-map (kbd "C-c i u") 'org-jira-update-issue)
;; (define-key org-jira-map (kbd "C-c i w") 'org-jira-progress-issue)

;; (define-key org-jira-map (kbd "C-c p g") 'org-jira-get-projects)

;; (define-key org-jira-map (kbd "C-c s c") 'org-jira-create-subtask)
;; (define-key org-jira-map (kbd "C-c s g") 'org-jira-get-subtasks)

;; (define-key org-jira-map (kbd "C-c t j") 'org-jira-todo-to-jira)

;; (define-key org-jira-map (kbd "C-c w u") 'org-jira-update-worklogs-from-org-clocks)


;; === Custom ===
(setq org-jira-custom-jqls
      '(
        (:jql " project = OP AND status = \"In Progress\" AND labels = Team2 order by priority DESC "
              :limit 100
              :filename "in_progress")
        (:jql " project = OP AND status = \"Selected for Development\" AND labels = Team2 order by priority DESC "
              :limit 100
              :filename "backlog")
        (:jql " project = OP AND issuetype = Epic AND labels = Team2 order by created DESC "
              :limit 100
              :filename "epics")))


;; (defconst org-jira-progress-issue-flow
;;   '(("To Do" . "In Progress"
;;     ("In Progress" . "Done"))))


(provide 'jira)

;;; jira.el ends here
