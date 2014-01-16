;; Custom startup file for Windows NT.
;;
;; Bence Kalmar

;;;; Functions

(defun init-toggle-fullscreen ()
  "Toggle fullscreen and return t, or return nil if it cannot be toggled."
  (interactive)
  (w32-send-sys-command #xf020)
  (w32-send-sys-command #xf030)
  t)

(defun init-auto-convert-lineending ()
  "If buffer's file's lineendings are not LF, convert them after user
confirmation.

Confirmation is controlled by `auto-convert-lineending-action'."
  (let (coding-new
        (coding-old (symbol-name buffer-file-coding-system)))
    (and 
     (string-match "-\\(?:unix\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-dos"))
     (init-auto-convert-lineending-confirm
      (format "Current coding is %s. Convert to %s? " coding-old coding-new))
     (set-buffer-file-coding-system (intern coding-new)))))
