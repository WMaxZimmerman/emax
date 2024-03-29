(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-unset-key "\C-d")
(global-set-key (kbd "C-d") 'duplicate-current-line-or-region)

(defun fzl-create-empty-file-if-no-exists(filePath)
   "Create a file with FILEPATH parameter."
   (if (file-exists-p filePath)
       (message (concat  "File " (concat filePath " already exists")))
     (with-temp-buffer (write-file filePath))))


(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun pair-mode ()
  "Increases font size and displays line numbers"
  (interactive)
  (text-scale-increase 2)
  (setq display-line-numbers t))

(defun un-pair-mode ()
  "Decreases font size and removes line numbers"
  (interactive)
  (text-scale-increase 0)
  (setq display-line-numbers nil))
