;; Custom startup file for GNU/Linux.
;; 
;; Bence Kalmar

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
    nil))

(defun auto-convert-lineending ()
  "If buffer's file's lineendings are not LF, convert them after user
confirmation.

Confirmation is controlled by `auto-convert-lineending-action'."
  (let (coding-new
        (coding-old (symbol-name buffer-file-coding-system)))
    (and 
     (string-match "-\\(?:dos\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-unix"))
     (auto-convert-lineending-confirm
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
       (message "Made executable %s" buffer-file-name)))

;; Not needed on windows.
(defun integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (setq x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil))

;;;; Hooks and similar

;; after buffer is saved to file
(add-hook 'after-save-hook 'auto-make-executable t)

;;; visual
(setq visible-bell t)

;; integrate clipboard
(integrate-clipboard)
