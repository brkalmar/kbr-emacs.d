;; Custom startup file.
;;
;; 2014  Bence Kalmar


(defvar bkalmar/home-directory (file-name-as-directory (expand-file-name "~"))
  "User home directory \"~\".")

(defvar bkalmar/emacs-directory (expand-file-name user-emacs-directory)
  "Base emacs.d directory.")

(defvar bkalmar/emacs-config-directory
  (concat bkalmar/emacs-directory "config/")
  "Directory for all packages' config/history/etc. files.")

(defvar bkalmar/emacs-elisp-directory
  (concat bkalmar/emacs-directory "elisp/")
  "Directory for elisp files to include from `init.el`.")


(load-file (concat bkalmar/emacs-elisp-directory "packages.el"))

(require 'f)
(dolist (file (f-entries (concat bkalmar/emacs-elisp-directory "packages/")))
  (load-file file))

(load-file (concat bkalmar/emacs-elisp-directory "backup.el"))

(load-file (concat bkalmar/emacs-elisp-directory "behavior.el"))

(load-file (concat bkalmar/emacs-elisp-directory "convenience.el"))

(load-file (concat bkalmar/emacs-elisp-directory "enabled-commands.el"))

(load-file (concat bkalmar/emacs-elisp-directory "fonts.el"))

(load-file (concat bkalmar/emacs-elisp-directory "visuals.el"))

(load-file (concat bkalmar/emacs-elisp-directory "keybindings.el"))

;; emacs customize
;;; NOTE: This is not loaded on purpose; all customization is done manually.
(customize-set-variable 'custom-file
                        (concat bkalmar/emacs-config-directory "custom.el"))
