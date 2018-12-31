(require 'linum)
(require 'zenburn-theme)

(zenburn-with-color-variables
  (set-face-attribute
   'linum nil
   :background zenburn-bg :foreground zenburn-fg :height 0.9 :width 'condensed))

(defvar bkalmar/linum-fmt-str "%01d"
  "Format string used by `bkalmar/linum-format'.")

(defun bkalmar/linum-before-numbering ()
  "Set `bkalmar/linum-fmt-str' to zero-padded format string."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (setq bkalmar/linum-fmt-str (concat "%0" (number-to-string w) "d"))))

(defun bkalmar/linum-format (line)
  "Format line using the format string `bkalmar/linum-fmt-str'."
  (propertize (format bkalmar/linum-fmt-str line) 'face 'linum))

(add-hook 'linum-before-numbering-hook 'bkalmar/linum-before-numbering t)
(customize-set-variable 'linum-format 'bkalmar/linum-format)
