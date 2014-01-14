;; Custom startup file for GNU/Linux.
;; 
;; 2013-09-17 / 2014-01-13
;; AlbusDrachir

;;; Functions

(defun toggle-fullscreen ()
  "Toggle fullscreen and return t, or return nil if it cannot be toggled."
  (interactive)
  (if (equal window-system 'x)
      (progn (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                    '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
             (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                    '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
             t)
    nil
    )
  )

(defun auto-convert-lineending ()
  "Check whether buffer's file's lineendings are not LF, and if so, ask the user
whether to convert it.

Confirmation is controlled by `auto-convert-lineending-confirm'.  If it is
\"always\", always convert without confirmation.  If it is \"never\", never
convert without confirmation.  If anything else, always ask for confirmation.
This variable can be changed during confirmation."
  (let (coding-new
        (coding-old (symbol-name buffer-file-coding-system)))
    (and 
     (string-match "-\\(?:dos\\|mac\\)$" coding-old)
     (setq coding-new
           (concat (substring coding-old 0 (match-beginning 0)) "-unix"))
     (confirm-convert
      (format "Current coding is %s. Convert to %s? " coding-old coding-new))
     (set-buffer-file-coding-system (intern coding-new)))))

;; This is unnecessary on windows, as files don't have an executable property.
(defun auto-make-executable ()
  "Make current buffer's file executable if begins whith a shebang."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (concat "chmod u+x "
                              (shell-quote-argument buffer-file-name)))
       (message "Made executable %s" buffer-file-name))
  )

;; Not needed on windows.
(defun integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (setq x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil
    )
  )

;;; Modes and extensions

;; TODO remove
;; Load CEDET.  See cedet/common/cedet.info for configuration details.
;; IMPORTANT: you must place this *before* any CEDET component (including EIEIO)
;; gets activated by another package (Gnus, auth-source, ...).
;; 2013-07-15
;; http://cedet.sourceforge.net/
;; (load-file "~/.emacs.d/cedet-bzr/trunk/cedet-devel-load.el")

;; TODO remove
;; enable semantic
;; (semantic-mode 1)

;; TODO remove
;; enable ede (project management) features
;; (global-ede-mode 1)

;; TODO remove
;; Add further minor-modes to be enabled by semantic-mode.  See doc-string of
;; `semantic-default-submodes' for other things you can use here.
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode
;;              t)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; json-mode
;; 2013-09-17
;; https://github.com/joshwnj/json-mode
;; (add-to-list 'load-path "~/.emacs.d/json-mode-master" t)
;; (require 'json-mode)

;; fill-column-indicator
;; 2013-09-17
;; http://www.emacswiki.org/emacs/FillColumnIndicator
;; (add-to-list 'load-path "~/.emacs.d/fill-column-indicator" t)
;; (require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "#253035")

;; modes for which fci is useful
(add-hook 'text-mode-hook 'fci-mode t)
(add-hook 'c-mode-hook 'fci-mode t)
(add-hook 'python-mode-hook 'fci-mode t)
(add-hook 'emacs-lisp-mode-hook 'fci-mode t)
(add-hook 'java-mode-hook 'fci-mode t)
(add-hook 'autoconf-mode-hook 'fci-mode t)
(add-hook 'sh-mode-hook 'fci-mode t)
(add-hook 'lua-mode-hook 'fci-mode t)
(add-hook 'jam-mode-hook 'fci-mode t)
(add-hook 'c++-mode-hook 'fci-mode t)
(add-hook 'nxml-mode-hook 'fci-mode t)
(add-hook 'makefile-mode-hook 'fci-mode t)

;; C mode
(setq-default c-basic-offset 4)

;; Lua mode
;; 2013-09-28
;; https://github.com/immerrr/lua-mode
;; (add-to-list 'load-path "~/.emacs.d/lua-mode-master" t)
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode) t)
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode) t)
(setq lua-indent-level 2)

;; Jam mode
;; 2013-11-22
;; https://github.com/emacsmirror/jam-mode
;; (add-to-list 'load-path "~/.emacs.d/jam-mode" t)
;; (require 'jam-mode)

;;; Keybindings
;;; `C-c [A-Za-z]' is reserved for users

;; window-resizing
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; misc
(global-set-key (kbd "C-c i") 'insert-info-comment)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)

;;; Hooks and similar

;; after a new frame is made
(add-hook 'after-make-frame-functions 'custom-after-make-frame t)

;; before buffer is saved to file
(add-hook 'before-save-hook 'update-modification-date t)
(add-hook 'before-save-hook 'auto-convert-lineending t)
(setq require-final-newline t)

;; after buffer is saved to file
(add-hook 'after-save-hook 'auto-make-executable t)

;;; Themes

(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'dark-emacs t)

;;; Enabled commands

(put 'downcase-region 'disabled nil)

;;; MISC.

;; coding
(prefer-coding-system 'utf-8-unix)

;; backup
(setq backup-directory-alist `(("." . "~/.emacs.d/backup/files/gnu-linux"))
      backup-by-copying t
      version-control t
      kept-new-versions 2
      kept-old-versions 0
      delete-old-versions t)
;; remove backups older than 30 days
(rm-old-backups (days-to-time 30))

;; auto-save
(setq auto-save-list-file-prefix "~/.emacs.d/backup/auto-save-list/.saves-")

;; turn icomplete mode on
(icomplete-mode 1)

;; visual
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq visible-bell t)

;; selections
(transient-mark-mode 1)
(delete-selection-mode 1)

;; integrate clipboard
(integrate-clipboard)

;; visualize size of buffer
(size-indication-mode 1)

;; numbers of columns to fill
(setq-default fill-column 80)

;; line and column numbers
(line-number-mode -1)
(column-number-mode 1)

(add-hook 'text-mode-hook 'linum-mode t)
(add-hook 'c-mode-hook 'linum-mode t)
(add-hook 'python-mode-hook 'linum-mode t)
(add-hook 'emacs-lisp-mode-hook 'linum-mode t)
(add-hook 'java-mode-hook 'linum-mode t)
(add-hook 'autoconf-mode-hook 'linum-mode t)
(add-hook 'sh-mode-hook 'linum-mode t)
(add-hook 'lua-mode-hook 'linum-mode t)
(add-hook 'jam-mode-hook 'linum-mode t)
(add-hook 'c++-mode-hook 'linum-mode t)
(add-hook 'nxml-mode-hook 'linum-mode t)
(add-hook 'makefile-mode-hook 'linum-mode t)

;; hide show mode
(add-hook 'c-mode-hook 'hs-minor-mode t)
(add-hook 'python-mode-hook 'hs-minor-mode t)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode t)
(add-hook 'java-mode-hook 'hs-minor-mode t)
(add-hook 'autoconf-mode-hook 'hs-minor-mode t)
(add-hook 'sh-mode-hook 'hs-minor-mode t)
(add-hook 'lua-mode-hook 'hs-minor-mode t)
(add-hook 'jam-mode-hook 'hs-minor-mode t)
(add-hook 'c++-mode-hook 'hs-minor-mode t)
(add-hook 'nxml-mode-hook 'hs-minor-mode t)
(add-hook 'makefile-mode-hook 'hs-minor-mode t)

;; indentation
(setq-default indent-tabs-mode nil)

;; comments
(setq comment-multi-line t)
