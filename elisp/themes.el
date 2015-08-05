;; Themes
;;
;; 2015  Bence Kalmar

(customize-set-variable 'custom-theme-directory
                        (concat user-emacs-directory "themes/"))
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
