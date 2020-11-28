(require 'f)
(require 'racer)
(require 'rust-mode)

(add-hook 'rust-mode-hook #'racer-mode)

(define-key racer-mode-map (kbd "C-c d") 'racer-describe)

;; NOTE: Workaround until emacs-racer is updated to recognize the new directory
;; layout.
(customize-set-variable
 'racer-rust-src-path
 (f-join "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
         "lib/rustlib/src/rust/library"))
