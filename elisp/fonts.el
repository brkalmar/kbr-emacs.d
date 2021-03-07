;; Themes
;;
;; 2015  Bence Kalmar

(defvar bkalmar/preferred-fonts-monospace
  '("DejaVu Sans Mono" "Unifont" "VL Gothic" "Liberation Mono" "FreeMono"
    "Andale Mono" "Droid Sans Mono")
  "List of preferred monospace fonts, in descending order of preference.")

(defvar bkalmar/preferred-fonts-proportional
  '("DejaVu Sans" "Arial" "VL PGothic" "Linux Biolinum O" "Droid Sans"
    "FreeSans" "Trebuchet MS" "Lato" "Liberation Sans" "Carlito")
  "List of preferred proportional fonts, in descending order of preference.")

(defvar bkalmar/preferred-fonts-set nil
  "Keeps track of whether the preferred fonts had been set already.")

(defun bkalmar/preferred-fonts-set (new-frame)
  "Set current and default fonts to the preferred fonts for every frame.

Does so only if `bkalmar/preferred-fonts-set' is nil and the font family list is
available; then sets `bkalmar/preferred-fonts-set' to non-nil.

The `new-frame' argument is ignored."
  (when (and (null bkalmar/preferred-fonts-set) (font-family-list))
    ;; default & fixed-pitch font
    (catch 'break
      (dolist (font-family bkalmar/preferred-fonts-monospace)
        (when (member font-family (font-family-list))
          (set-face-attribute 'default nil :family font-family)
          (set-face-attribute 'fixed-pitch nil :family font-family)
          (throw 'break t))))
    ;; variable-pitch font
    (catch 'break
      (dolist (font-family bkalmar/preferred-fonts-proportional)
        (when (member font-family (font-family-list))
          (set-face-attribute 'variable-pitch nil :family font-family)
          (throw 'break t))))
    (setq bkalmar/preferred-fonts-set t)))

(add-hook 'after-make-frame-functions 'bkalmar/preferred-fonts-set t)
