;; Custom startup file.
;;
;; 2014  Bence Kalmar


;; This must come before configurations of installed packages.
(package-initialize)

(defvar bkalmar/userdir nil
  "Real user directory, \"~\" in GNU/Linux, not necessarily \"~\" in Windows.

Should be set in OS-specific files.")

(defvar bkalmar/emacs-config-directory (concat user-emacs-directory "config/")
  "Directory for all packages' config/history/etc. files.")

(defvar bkalmar/emacs-elisp-directory (concat user-emacs-directory "elisp/")
  "Directory for elisp files to include in `init.el`.")

(load-file (concat bkalmar/emacs-elisp-directory "packages.el"))

(cond
 ((equal system-type 'gnu/linux)
  (load-file (concat bkalmar/emacs-elisp-directory "gnu-linux/init.el")))
 ((equal system-type 'windows-nt)
  (load-file (concat bkalmar/emacs-elisp-directory "windows-nt/init.el")))
 (t
  (message "Could not find appropriate config file for system type: ‘%s’"
           system-type)))

(when (string-prefix-p "/uio/" (getenv "HOME"))
  (load-file (concat bkalmar/emacs-elisp-directory "uio/init.el")))

(load-file (concat bkalmar/emacs-elisp-directory "backup.el"))

(load-file (concat bkalmar/emacs-elisp-directory "copyright-comment.el"))

(load-file (concat bkalmar/emacs-elisp-directory "convenience.el"))

(load-file (concat bkalmar/emacs-elisp-directory "font-info.el"))

(load-file (concat bkalmar/emacs-elisp-directory "terminal.el"))

(load-file (concat bkalmar/emacs-elisp-directory "wordprocessor-mode.el"))

;; Packages & modes
(require 'f)
(dolist (file (f-entries (concat bkalmar/emacs-elisp-directory "packages/")))
  (load-file file))

(load-file (concat bkalmar/emacs-elisp-directory "enabled-commands.el"))

(load-file (concat bkalmar/emacs-elisp-directory "themes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "programming-modes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "visuals.el"))

(load-file (concat bkalmar/emacs-elisp-directory "keybindings.el"))

;; before buffer is saved to file
(customize-set-variable 'require-final-newline t)

;; coding
(prefer-coding-system 'utf-8)

;; quoted-insert base 10
(customize-set-variable 'read-quoted-char-radix 10)

;; indentation
(customize-set-variable 'indent-tabs-mode nil)

;; comments
(customize-set-variable 'comment-multi-line t)

;; locale
(setq system-time-locale "C")

;; emacs customize
;;; NOTE: This is not loaded on purpose; all customization is done manually.
(customize-set-variable 'custom-file
                        (concat bkalmar/emacs-config-directory "custom.el"))
