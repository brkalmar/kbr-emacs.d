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

Confirmation is controlled by `init-auto-convert-lineending-action'."
  (let (coding-new
        (coding-old (symbol-name buffer-file-coding-system)))
    (and
     (not (init-auto-convert-lineending-skip))
     (string-match "-\\(?:unix\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-dos"))
     (init-auto-convert-lineending-confirm
      (format "Current coding is %s. Convert to %s? " coding-old coding-new))
     (set-buffer-file-coding-system (intern coding-new)))))

;; set diff & ediff commands to specified file if it exists
(let ((diff-exec-file "C:\\Users\\be02029606\\_\\git-1.8.4\\bin\\diff.exe"))
  (when (file-readable-p diff-exec-file)
    (setq diff-command diff-exec-file)
    (setq ediff-diff-program diff-exec-file)))
