(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

(customize-set-variable
 'ac-comphist-file
 (expand-file-name
  (concat bkalmar/emacs-config-directory "auto-complete/ac-comphist.dat")))
(mkdir (file-name-directory ac-comphist-file) t)

;; custom sources
(dolist (elem '(c-mode-hook
                c++-mode-hook
                python-mode-hook))
  (add-hook
   elem
   (lambda () (setq ac-sources (append ac-sources '(ac-source-semantic))))))

(global-auto-complete-mode t)
