;; Custom startup file for GNU/Linux.
;; Uses `display-*-p' where possible.
;; 
;; 2013-09-17 / 2014-01-13
;; AlbusDrachir

;;;; Packages

(load-file "~/.emacs.d/init-packages.el")

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

(setq auto-convert-lineending-confirm t)

(defun confirm-convert (prompt)
  "Ask user for confirmation.  Append answer options to PROMPT."
  (cond
   ((equal auto-convert-lineending-confirm "always")
    t)
   ((equal auto-convert-lineending-confirm "never")
    nil)
   (t
    (let (answer
          res
          (found nil))
      (while (not found)
        (setq found t)
        (setq answer (read-from-minibuffer
                      (concat prompt "(y, n, always or never) ")))
        (cond 
         ((equal answer "y")
          (setq res t))
         ((equal answer "n")
          (setq res nil))
         ((equal answer "always")
          (setq auto-convert-lineending-confirm "always")
          (setq res t))
         ((equal answer "never")
          (setq auto-convert-lineending-confirm "never")
          (setq res nil))
         (t
          (setq found nil))))
      res))))

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

(defun update-modification-date ()
  "Update the last modification date of current buffer's file if it contains the
string `YEAR-MN-DY / YEAR-MN-DY'."
  (interactive)
  (setq date-regexp "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
  (setq current-date (format-time-string "%Y-%m-%d"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (and
           (re-search-forward (format "%s / %s" date-regexp date-regexp) nil t)
           (not (equal (match-string-no-properties 2) current-date)) )
          (progn (replace-match (concat "\\1 / " current-date) nil nil)
                 (message "Updated modification date to %s" current-date))
        )
      )
    )
  )

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

(defun get-cm-dates ()
  "Return the creation and modification date (today) as a string in format
'YYYY-MM-DD / YYYY-MM-DD'."
  (let (
        (date (format-time-string "%Y-%m-%d")))
    (concat date " / " date))
  )

(setq info-name "AlbusDrachir")

(defun insert-info-comment ()
  "Insert an info comment.

The comment is muli-line if possible.  It consists of 4 lines: 2 empty ones, one
inserted by 'insert-cm-dates' and one with a name.  After insertion, point is
positioned at the beginning of the first line.

The name is `info-name' (must be a string)."
  (interactive)
  (let ((comment-style 'multi-line)
        (start (point))
        (first-line nil))
    (insert (format "X\nX\n%s\n%s\n\n" (get-cm-dates) info-name))
    (backward-char 2)
    (comment-region start (point))
    (goto-char start)
    (search-forward "\n")
    (backward-char 2)
    (delete-char 1)
    (setq first-line (point))
    (forward-char)
    (search-forward "\n")
    (backward-char 2)
    (delete-char 1)
    (goto-char first-line))
  )

(defun set-custom-colors ()
  "Set background and foreground colours if possible and return t. Return nil if
not possible."
  (interactive)
  (if (display-color-p)
      (progn (set-background-color "#000000")
             (set-foreground-color "#EEEEEE")
             t)
    nil
    )
  )

(defun custom-after-make-frame (new-frame)
  "Change colors and toggle fullscreen."
  (select-frame new-frame)
  (set-custom-colors)
  (toggle-fullscreen)
  (when (display-graphic-p)
      (scroll-bar-mode -1)
    )
  )

(defun rm-old-backups (age)
  "Remove all backup files whose modification time is older than AGE, in the
directory associated to \".\" in `backup-directory-alist'.  AGE must be one of
the three time formats described in 'replace.el'."
  (let
      ((count 0)
       (old-before (time-subtract (current-time) age))
       (dir (cdr (assoc "." backup-directory-alist))))
    (and
     (file-directory-p dir)
     (file-readable-p dir)
     (dolist (filename (directory-files dir t) t)
       (and
        (file-regular-p filename)
        (file-writable-p filename)
        ;; mod-time less than 'old-before'
        (time-less-p (nth 5 (file-attributes filename)) old-before)
        (not (delete-file filename))
        (setq count (1+ count))))
     (message "Removed %d old backup files" count))))

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
;; Source:
;; https://github.com/suvayu/.emacs.d/blob/master/themes/dark-emacs-theme.el
(load-theme 'dark-emacs)

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
