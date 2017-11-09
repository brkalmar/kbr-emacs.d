(require 'racer)
(require 'rust-mode)

(add-hook 'rust-mode-hook #'racer-mode)

(define-key racer-mode-map (kbd "C-c d") 'racer-describe)
