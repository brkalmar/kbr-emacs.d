;; Visual improvements
;;
;; 2015  Bence Kalmar

(require 'f)

(defun bkalmar/init-visuals ()
  "Toggle fullscreen; disappear scrollbar."
  (interactive)
  (toggle-frame-maximized)
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    ;; 80 columns should comfortably fit on small screens
    (when (< (display-pixel-width) 1400)
      (set-face-attribute 'default nil :height 90)))
  ;; temporary fix for Debian
  (set-cursor-color "#ffcc00"))

(bkalmar/init-visuals)

(defun bkalmar/after-make-frame (new-frame)
  "Call `bkalmar/init-visuals' on `new-frame'."
  (select-frame new-frame)
  (bkalmar/init-visuals))

(add-hook 'after-make-frame-functions 'bkalmar/after-make-frame t)

(customize-set-variable 'inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(size-indication-mode 1)
(menu-bar-mode -1)

;; frame resize
(customize-set-variable 'frame-resize-pixelwise t)

;; default fringes on both sides
(fringe-mode nil)

;; selections
(transient-mark-mode 1)
(delete-selection-mode 1)

;; numbers of columns to fill
(customize-set-variable 'fill-column 80)

;; line and column numbers
(line-number-mode -1)
(column-number-mode 1)

;; frame & icon titles

(defun bkalmar/filename-reverse (filename &optional sep)
  "Return string of FILENAME with its elements in reverse order, or nil if
FILENAME is nil.

If SEP is non-nil, it is used to separate the elements instead of the default
path separator."
  (when filename
    (let ((separator (if (null sep) (f-path-separator) sep)))
      (mapconcat 'identity
                 (reverse (split-string filename (f-path-separator)))
                 separator))))

(setq frame-title-format
      '((:eval (or (bkalmar/filename-reverse buffer-file-truename "\\") "%b"))
        " (%I)"))

(setq icon-title-format "%b")

;; themes

(customize-set-variable 'custom-theme-directory
                        (concat bkalmar/emacs-directory "themes/"))

(load-theme 'zenburn t)
(load-theme 'bkalmar t)

;; scratch

(customize-set-variable 'initial-scratch-message nil)
