(setq dnd-srd-dir "~/.emacs.d/ignore/org-dnd-srd/")
(setq dnd-srd-dir "~/.emacs.d/ignore/org-dnd/")
(setq dnd-snippet-dir "~/.emacs.d/ignore/dnd-mode/snippets")

(setq dnd-org-capture-templates `(("i" "Inbox" entry  (file "inbox.org")
                                   ,(concat "* TODO %?\n"
                                            "/Entered on/ %U"))
                                  ("n" "Note" entry  (file "notes.org")
                                   ,(concat "* %?\n"
                                            "/Entered on/ %U"))))
