(require 'rust-mode)

(customize-set-variable 'rust-format-on-save t)

(add-hook 'rust-mode-hook
          (lambda () (whitespace-mode 'toggle)
            (set-fill-column 100)
            (whitespace-mode 'toggle)))
