(setq dnd-srd-dir "~/Dropbox/org-dnd-srd/")
(setq dnd-snippet-dir "~/Dropbox/dnd-mode/snippets")

(setq dnd-org-capture-templates `(("i" "Inbox" entry  (file "inbox.org")
                                   ,(concat "* TODO %?\n"
                                            "/Entered on/ %U"))
                                  ("n" "Note" entry  (file "notes.org")
                                   ,(concat "* %?\n"
                                            "/Entered on/ %U"))))
