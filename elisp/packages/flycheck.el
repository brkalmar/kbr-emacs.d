(require 'flycheck)

(customize-set-variable 'flycheck-indication-mode 'right-fringe)

(add-hook 'after-init-hook #'global-flycheck-mode)
