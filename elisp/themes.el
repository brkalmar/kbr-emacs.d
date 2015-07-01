;; Themes
;;
;; 2015  Bence Kalmar

(setq custom-theme-directory (concat user-emacs-directory "themes"))
(load-theme 'zenburn t)
(load-theme 'bkalmar t)

;; default & fixed-pitch font

(defvar bkalmar/preferred-fonts-monospace nil
  "List of preferred monospace fonts, in descending order of preference.

Should be set in OS-specific files.")

(catch 'break
  (dolist (font-family bkalmar/preferred-fonts-monospace)
    (when (member font-family (font-family-list))
      (set-face-attribute 'default nil :family font-family)
      (set-face-attribute 'fixed-pitch nil :family font-family)
      (throw 'break t))))

;; variable-pitch font

(defvar bkalmar/preferred-fonts-proportional nil
  "List of preferred proportional fonts, in descending order of preference.

Should be set in OS-specific files.")

(catch 'break
  (dolist (font-family bkalmar/preferred-fonts-proportional nil)
    (when (member font-family (font-family-list))
      (set-face-attribute 'variable-pitch nil :family font-family)
      (throw 'break t))))

;; 80 columns should comfortably fit on small screens
(when (and (display-graphic-p) (< (display-pixel-width) 1400))
  (set-face-attribute 'default nil :height 90))
