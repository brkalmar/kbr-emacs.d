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

(load-file (concat bkalmar/emacs-elisp-directory "backup.el"))

(load-file (concat bkalmar/emacs-elisp-directory "convenience.el"))

(load-file (concat bkalmar/emacs-elisp-directory "font-info.el"))

(load-file (concat bkalmar/emacs-elisp-directory "terminal.el"))

;; Packages & modes
(require 'f)
(dolist (file (f-entries (concat bkalmar/emacs-elisp-directory "packages/")))
  (load-file file))

(load-file (concat bkalmar/emacs-elisp-directory "enabled-commands.el"))

(load-file (concat bkalmar/emacs-elisp-directory "themes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "programming-modes.el"))

(load-file (concat bkalmar/emacs-elisp-directory "visuals.el"))

(load-file (concat bkalmar/emacs-elisp-directory "keybindings.el"))


(defun bkalmar/auto-make-executable ()
  "Make current buffer's file executable if begins whith a shebang."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (concat "chmod u+x "
                              (shell-quote-argument buffer-file-name)))
       (message "Made executable %s" buffer-file-name)))

;; after buffer is saved to file
(add-hook 'after-save-hook 'bkalmar/auto-make-executable t)

(defun bkalmar/integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (customize-set-variable 'x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil))

;; integrate clipboard
(bkalmar/integrate-clipboard)

;;; visual
(customize-set-variable 'visible-bell t)

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

;; enabled commands
(put 'set-goal-column 'disabled nil)
