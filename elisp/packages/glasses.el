(require 'glasses)
(require 'zenburn-theme)

(customize-set-variable 'glasses-original-separator "")
(customize-set-variable 'glasses-separate-parentheses-p nil)
(customize-set-variable 'glasses-separator "")

(zenburn-with-color-variables
  (defface kbr/glasses-face
    `((t . (:underline ,zenburn-bg+3 :weight bold)))
    "Highlights captitals in `glasses-mode'."))

(customize-set-variable 'glasses-face 'kbr/glasses-face)
