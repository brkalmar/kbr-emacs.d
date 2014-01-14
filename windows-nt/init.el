;; Custom startup file for Windows NT.
;;
;; 2013-09-17 / 2014-01-07
;; AlbusDrachir

;;; Functions

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

;;; Hooks and similar

;; after a new frame is made
(add-hook 'after-make-frame-functions 'custom-after-make-frame t)

;; before buffer is saved to file
(add-hook 'before-save-hook 'update-modification-date t)
(add-hook 'before-save-hook 'auto-convert-lineending t)
(setq require-final-newline t)

;;; Themes

(setq custom-theme-directory "~/.emacs.d/themes")

;;; Enabled commands

(put 'downcase-region 'disabled nil)

;;; MISC.

;; coding
(prefer-coding-system 'utf-8-dos)

;; backup
(setq backup-directory-alist `(("." . "~/.emacs.d/file-backups/windows-nt"))
      backup-by-copying t
      version-control t
      kept-new-versions 2
      kept-old-versions 0
      delete-old-versions t)
;; remove backups older than 30 days
(rm-old-backups (days-to-time 30))

;; turn icomplete mode on
(icomplete-mode 1)

;; visual
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)

;; synchronize with `custom-after-make-frame'
(set-custom-colors)
(toggle-fullscreen)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  )

;; selections
(transient-mark-mode 1)
(delete-selection-mode 1)

;; visualize size of buffer
(size-indication-mode 1)

;; numbers of columns to fill
(setq-default fill-column 80)

;; line and column numbers
(line-number-mode -1)
(column-number-mode 1)

;; indentation
(setq-default indent-tabs-mode nil)
