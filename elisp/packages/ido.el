(require 'ido)

(customize-set-variable 'ido-save-directory-list-file
                        (concat bkalmar/emacs-config-directory "ido/ido.last"))
(mkdir (file-name-directory ido-save-directory-list-file) t)

(ido-mode 1)
