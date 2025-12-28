;;; early-init.el -*- lexical-binding: t; -*-

;;;; Quiet startup warnings, restore after startup

(defvar max/original-warning-minimum-level warning-minimum-level
  "Saved warning minimum level before we silence warnings during startup.")

(defvar max/suppress-startup-noise t
  "When non-nil, suppress noisy startup warnings/messages.")

;; Silence warnings during startup
(setq warning-minimum-level :error)

;; Filter noisy messages (covers rare cases that bypass warning filters)
(defun max/message-filter (orig fmt &rest args)
  (let ((text (when (stringp fmt) (apply #'format fmt args))))
    (if (and max/suppress-startup-noise
             text
             (string-match-p "Missing [`']lexical-binding[`'] cookie" text))
        nil
      (apply orig fmt args))))

(advice-add 'message :around #'max/message-filter)

;; Restore normal behavior after startup finishes
(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; Restore warnings
   (setq warning-minimum-level max/original-warning-minimum-level)
   ;; Stop filtering messages
   (setq max/suppress-startup-noise nil)))
