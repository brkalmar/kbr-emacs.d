;; Custom startup file for Windows NT.
;;
;; 2013-09-17 / 2014-01-07
;; AlbusDrachir

;;;; Functions

(defun toggle-fullscreen ()
  "Toggle fullscreen and return t, or return nil if it cannot be toggled."
  (interactive)
  (w32-send-sys-command #xf020)
  (w32-send-sys-command #xf030)
  t
  )

(defun auto-convert-lineending ()
  "Check whether buffer's file's lineendings are not LF, and if so, ask the user
whether to convert it.

Confirmation is controlled by `auto-convert-lineending-confirm'.  If it is
\"always\", always convert without confirmation.  If it is \"never\", never
convert without confirmation.  If anything else, always ask for confirmation.
This variable can be changed during confirmation."
  (let (coding-new
        (coding-old (symbol-name buffer-file-coding-system)))
    (and 
     (string-match "-\\(?:unix\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-dos"))
     (confirm-convert
      (format "Current coding is %s. Convert to %s? " coding-old coding-new))
     (set-buffer-file-coding-system (intern coding-new)))))
