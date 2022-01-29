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

(provide 'misc)

;;; misc.el ends here
