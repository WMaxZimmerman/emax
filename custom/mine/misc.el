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

(defun org-export-json ()
  "Tangle JSON blocks into files named using their level 1 and level 2 headers."
  (interactive)
  (let ((org-tree (org-element-parse-buffer)))
    (org-element-map org-tree 'headline
      (lambda (lvl1)
        (when (= 1 (org-element-property :level lvl1))
          (let ((h1 (org-element-property :raw-value lvl1)))
            (org-element-map (org-element-contents lvl1) 'headline
              (lambda (lvl2)
                (when (= 2 (org-element-property :level lvl2))
                  (let ((h2 (org-element-property :raw-value lvl2)))
                    ;; Build slugified name
                    (let* ((slug (lambda (s)
                                   (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" s))))
                           (filename (format "%s-%s.json" (funcall slug h1) (funcall slug h2))))
                      ;; Search for JSON block
                      (org-element-map (org-element-contents lvl2) 'src-block
                        (lambda (block)
                          (when (string= (org-element-property :language block) "json")
                            (let ((json-content (org-element-property :value block)))
                              (with-temp-file filename
                                (insert json-content))
                              (message "Exported: %s" filename))))
                        nil t))))))))))))

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
