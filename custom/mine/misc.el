;;; package --- misc
;;; Commentary:
;;; This is just a test of creating some function.

;;; Code:

(defun meme ()
  "tHiS MaKeS ThE CaSe oF ThE HiGhLiGhTeD StRiNg bEtTeR"
  (interactive)

  (setq text (buffer-substring (region-beginning) (region-end)))
  (setq chars (mapcar 'string text))
  (setq i 0)
  (setq new-text (mapconcat
                   (lambda (char)
                     (progn (setq i (+ i 1))
                            (if (eq (% i 2) 0)
                                (upcase char)
                              (downcase char))
                            ))
                   chars ""))

  (kill-region (region-beginning) (region-end))
  (insert new-text))

(defun lithp ()
  "Thith function will write thingth with a lithp"
  (interactive)

  (setq text (buffer-substring (region-beginning) (region-end)))
  (setq chars (mapcar 'string text))
  (setq new-text (mapconcat
                   (lambda (char)
                     (if (string= "s" char)
                         "th"
                       (if (string= "S" char)
                           "Th"
                         char)))
                   chars ""))

  (kill-region (region-beginning) (region-end))
  (insert new-text))

(defun org-eval-buffer ()
  "Evaluates the current org buffer"
  (interactive)

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "elisp")))  ;don't ask for ditaa
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
  
  (setq starting-point (point))
  (outline-show-all)
  (goto-char (point-min))

  

  (unwind-protect
      (while (string= "1" "1")
        (search-forward "BEGIN_SRC")
        (org-babel-execute-src-block)
        (search-forward ":results:")
        (next-line)
        (backward-word)
        
        (org-ctrl-c-ctrl-c))
    (unwind-protect
        (while (string= "1" "1")
          (search-forward "TBLFM")
          (org-ctrl-c-ctrl-c))
      (progn 
        (goto-char (point-min))
        (search-forward "* Constants")
        (outline-hide-leaves)
        (goto-char starting-point)))))

(provide 'misc)

;;; misc.el ends here
