;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))

(require 'flycheck-plantuml)
(require 'ob)
(require 'ob-core)

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/tools/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(flycheck-plantuml-setup)
