(require 'rust-mode)

;; NOTE: We always use stable rustfmt for now, as it is not available on some
;; nightly toolchains.
(customize-set-variable
 'rust-rustfmt-switches
 (cons "+stable" rust-rustfmt-switches))

(customize-set-variable 'rust-format-on-save t)

(add-hook 'rust-mode-hook
          (lambda () (whitespace-mode 'toggle)
            (set-fill-column 100)
            (whitespace-mode 'toggle)))
