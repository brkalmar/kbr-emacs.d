;; Custom startup file.
;;
;; 2014  Bence Kalmar

(defvar bkalmar/userdir nil
  "Real user directory, \"~\" in GNU/Linux, not necessarily \"~\" in Windows.

Should be set in OS-specific files.")

(defvar bkalmar/emacs-config-directory (concat user-emacs-directory ".config/")
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

(load-file (concat bkalmar/emacs-elisp-directory "backup.el"))

(load-file (concat bkalmar/emacs-elisp-directory "copyright-comment.el"))

(load-file (concat bkalmar/emacs-elisp-directory "convenience.el"))

(load-file (concat bkalmar/emacs-elisp-directory "font-info.el"))

;; Packages & modes
(require 'f)
(dolist (file (f-entries (concat bkalmar/emacs-elisp-directory "packages/")))
  (load-file file))

(load-file (concat bkalmar/emacs-elisp-directory "enabled-commands.el"))

(load-file (concat bkalmar/emacs-elisp-directory "themes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "prose-like-modes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "programming-modes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "visuals.el"))

(load-file (concat bkalmar/emacs-elisp-directory "keybindings.el"))

;; before buffer is saved to file
(setq require-final-newline t)

;; coding
(prefer-coding-system 'utf-8)

;; quoted-insert base 10
(setq read-quoted-char-radix 10)

;; indentation
(setq-default indent-tabs-mode nil)

;; comments
(setq comment-multi-line t)

;; locale
(setq system-time-locale "C")
