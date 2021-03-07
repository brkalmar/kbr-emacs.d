(defvar kbr/home-directory (file-name-as-directory (expand-file-name "~"))
  "User home directory \"~\".")

(defvar kbr/emacs-directory (expand-file-name user-emacs-directory)
  "Base emacs.d directory.")

(defvar kbr/emacs-config-directory
  (concat kbr/emacs-directory "config/")
  "Directory for all packages' config/history/etc. files.")

(defvar kbr/emacs-elisp-directory
  (concat kbr/emacs-directory "elisp/")
  "Directory for elisp files to include from `init.el`.")

;;; packages

(load-file (concat kbr/emacs-elisp-directory "packages.el"))

(require 'f)
(dolist (file (f-entries (concat kbr/emacs-elisp-directory "packages/")))
  (load-file file))

;;; further configuration

(load-file (concat kbr/emacs-elisp-directory "backup.el"))

(load-file (concat kbr/emacs-elisp-directory "behavior.el"))

(load-file (concat kbr/emacs-elisp-directory "convenience.el"))

(load-file (concat kbr/emacs-elisp-directory "enabled-commands.el"))

(load-file (concat kbr/emacs-elisp-directory "fonts.el"))

(load-file (concat kbr/emacs-elisp-directory "visuals.el"))

(load-file (concat kbr/emacs-elisp-directory "keybindings.el"))

;;; emacs customize system

;; NOTE: This is not loaded on purpose; all customization is done manually.
(customize-set-variable 'custom-file
                        (concat kbr/emacs-config-directory "custom.el"))
