(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; (defun yaml-outline-level ()
;;   "Return the outline level based on the indentation, hardcoded at 2 spaces."
;;   (s-count-matches "[ ]\\{2\\}" (match-string 0)))

;; (defun yaml-mode-outline-hook ()
;;   (outline-minor-mode)
;;   (setq outline-regexp
;;         (rx
;;          (seq
;; 	      bol
;; 	      (group (zero-or-more "  ")
;; 	             (or (group
;; 		              (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
;; 			                   (seq "'" (*? (not (in "'" "\n"))) "'")
;; 			                   (*? (not (in ":" "\n"))))
;; 			               ":"
;; 			               (?? (seq
;; 			                    (*? " ")
;; 			                    (or (seq "&" (one-or-more nonl))
;; 				                    (seq ">-")
;; 				                    (seq "|"))
;; 			                    eol))))
;; 		             (group (seq
;; 			                 "- "
;; 			                 (+ (not (in ":" "\n")))
;; 			                 ":"
;; 			                 (+ nonl)
;; 			                 eol)))))))
;;   (setq outline-level 'yaml-outline-level))

;; (yaml-mode . yaml-mode-outline-hook)
