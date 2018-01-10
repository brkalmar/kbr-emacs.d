(require 'cc-mode)

(customize-set-variable 'c-basic-offset 4)

(add-to-list 'c-default-style '(c-mode . "k&r"))

(add-hook 'c-mode-hook (lambda () (setq indent-tabs-mode t)) t)
