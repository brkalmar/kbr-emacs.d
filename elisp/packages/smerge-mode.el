(require 'smerge-mode)
(require 'zenburn-theme)

(zenburn-with-color-variables
  (set-face-attribute 'smerge-base nil
                      :background zenburn-bg-1)
  (set-face-attribute 'smerge-markers nil
                      :background zenburn-bg+1)
  (set-face-attribute 'smerge-mine nil
                      :background zenburn-red-5)
  (set-face-attribute 'smerge-other nil
                      :background zenburn-green-5)
  (set-face-attribute 'smerge-refined-added nil
                      :background zenburn-green-4)
  (set-face-attribute 'smerge-refined-removed nil
                      :background zenburn-red-4))
