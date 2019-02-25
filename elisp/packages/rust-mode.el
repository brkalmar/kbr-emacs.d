(require 'rust-mode)

(customize-set-variable 'rust-format-on-save t)

(add-hook 'rust-mode-hook (lambda () (set-fill-column 100)))
