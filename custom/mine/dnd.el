;;; package --- dnd
;;; Commentary:
;;; This is just a test of creating some function.

;;; Code:

(defun rtd ()
  "Sometimes you just got to roll the dice."
  (interactive)

  (setq dnd-text (buffer-substring (region-beginning) (region-end)))
  (setq dnd-text-values (split-string dnd-text " "))
  (setq dnd-dice-text (nth 0 dnd-text-values))
  (setq dnd-mod-text (nth 2 dnd-text-values))
  
  (setq dnd-values (split-string dnd-dice-text "d"))
  (setq dnd-count (string-to-number (nth 0 dnd-values)))
  (setq dnd-limit (string-to-number (nth 1 dnd-values)))

  (setq dnd-total 0)
  (let ((dnd-x 0))
    (while (< dnd-x dnd-count)
      (setq dnd-rand (random dnd-limit))
      (setq dnd-roll (+ dnd-rand 1))
      (setq dnd-total (+ dnd-total dnd-roll))
      (message "Dice %d was %d" (+ 1 dnd-x) dnd-roll)
      (setq dnd-x (+ dnd-x 1))))

  (message "Mod was %s" dnd-mod-text)
  (if dnd-mod-text
      (setq dnd-total (+ dnd-total (string-to-number dnd-mod-text)))
    (nil))

  (message "You rolled %d" dnd-total))

(define-minor-mode dnd-mode
  "Manage and interact with character sheets"
  :lighter " dnd"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") 'rtd)
            map))

(defun calc-dnd-mod (score)
  "Calculates the modifier of a DND ability score"
  (message "input is: %d" score)
  (floor (- (/ score 2) 5)))

(provide 'dnd)

;;; dnd.el ends here
