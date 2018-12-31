(require 'hlinum)

(set-face-attribute
 'linum-highlight-face nil
 :inherit 'linum :background 'unspecified :foreground 'unspecified
 :inverse-video t :weight 'bold)

(hlinum-activate)
