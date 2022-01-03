;;; package --- dnd
;;; Commentary:
;;; This is just a test of creating some function.

;;; Code:

(defun rtd ()
  "Sometimes you just got to roll the dice."
  (interactive)

  (defvar dnd-text (buffer-substring (region-beginning) (region-end)))
  (defvar dnd-values (split-string dnd-text "d"))

  (defvar dnd-count (string-to-number (nth 0 dnd-values)))
  (defvar dnd-limit (string-to-number (nth 1 dnd-values)))
  
  (defvar dnd-x 0)
  (defvar dnd-total 0)

  (while (< dnd-x dnd-count)
    (defvar dnd-rand (random dnd-limit))
    (defvar dnd-roll (+ dnd-rand 1))
    (defvar dnd-total (+ dnd-total dnd-roll))
    (message "Dice %d was %d" (+ 1 dnd-x) dnd-roll)
    (defvar dnd-x (+ dnd-x 1)))
  
  (message "You rolled %d" dnd-total))

(define-minor-mode dnd-mode
  "Manage and interact with character sheets"
  :lighter " dnd"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") 'rtd)
            map))

(provide 'dnd)

;;; dnd.el ends here
