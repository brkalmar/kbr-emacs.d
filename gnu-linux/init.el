;; Custom startup file for GNU/Linux.
;; 
;; 2013-09-17 / 2014-01-13
;; AlbusDrachir

;;;; Functions

(defun toggle-fullscreen ()
  "Toggle fullscreen and return t, or return nil if it cannot be toggled."
  (interactive)
  (if (equal window-system 'x)
      (progn (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                    '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
             (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                    '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
             t)
    nil
    )
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
     (string-match "-\\(?:dos\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-unix"))
     (confirm-convert
      (format "Current coding is %s. Convert to %s? " coding-old coding-new))
     (set-buffer-file-coding-system (intern coding-new)))))

;; This is unnecessary on windows, as files don't have an executable property.
(defun auto-make-executable ()
  "Make current buffer's file executable if begins whith a shebang."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (concat "chmod u+x "
                              (shell-quote-argument buffer-file-name)))
       (message "Made executable %s" buffer-file-name))
  )

;; Not needed on windows.
(defun integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (setq x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil
    )
  )

;;;; Hooks and similar

;; after buffer is saved to file
(add-hook 'after-save-hook 'auto-make-executable t)

;;; visual
(setq visible-bell t)

;; integrate clipboard
(integrate-clipboard)
