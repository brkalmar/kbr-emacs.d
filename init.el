;; Custom startup file for GNU-emacs.
;;
;; Bence Kalmar

;;;; Packages

(load-file "~/.emacs.d/init-packages.el")

;;;; System-specific

(cond
 ((equal system-type 'gnu/linux)
  (message "Loading GNU/Linux specific init file...")
  (load-file "~/.emacs.d/gnu-linux/init.el"))
 ((equal system-type 'windows-nt)
  (message "Loading Windows NT specific init file...")
  (load-file "~/.emacs.d/windows-nt/init.el"))
 (t
  (message "Could not find appropriate config file for system type: %s"
           system-type)))

;;;; Functions & variables

(defvar init-auto-convert-lineending-skip-list
  '("~/.emacs.d/url/cookies")
  "A list of filenames for which no conversion is done when
`init-auto-convert-lineending' is called.")

(defun init-auto-convert-lineending-skip ()
  "Check all paths in `init-auto-convert-lineending-skip-list' against the
buffer file name.  Return non-nil if one of them equals the buffer file name,
nil otherwise."
  (let ((found nil)
        (filename (buffer-file-name)))
    (dolist (path init-auto-convert-lineending-skip-list found)
      (and
       (equal filename (expand-file-name path))
       (setq found t)))))

(defvar init-auto-convert-lineending-action "confirm"
  "Decides what `init-auto-convert-lineending' does.

If \"confirm\", the function asks the user whether to convert or not.
If \"always\", the function always converts, without asking the user.
If \"never\", the function never converts.")

(defun init-auto-convert-lineending-confirm (prompt)
  "Called by `init-auto-convert-lineending'."
  (cond
   ((equal init-auto-convert-lineending-action "confirm")
    (y-or-n-p prompt))
   ((equal init-auto-convert-lineending-action "always")
    t)
   ((equal init-auto-convert-lineending-action "never")
    nil)
   (t
    (error "Invalid value of `init-auto-convert-lineending-action': %s"
           init-auto-convert-lineending-action))))

(defun init-update-modification-date ()
  "Update the last modification date of current buffer's file if it contains the
string `YEAR-MN-DY / YEAR-MN-DY'."
  (interactive)
  (let ((date-regexp "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
        (current-date (format-time-string "%Y-%m-%d")))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (and
         (re-search-forward (format "%s / %s" date-regexp date-regexp) 1000 t)
         (not (equal (match-string-no-properties 2) current-date))
         (replace-match (concat "\\1 / " current-date) nil nil)
         (message "Updated modification date to %s" current-date))))))

(defun init-get-cm-dates ()
  "Return the creation and modification date (today) as a string in format
'YYYY-MM-DD / YYYY-MM-DD'."
  (let
      ((date (format-time-string "%Y-%m-%d")))
    (concat date " / " date)))

(defvar init-info-comment-name "Bence Kalmar"
  "The name used by `init-insert-info-comment'.  Must be a string.")

(defun init-insert-info-comment ()
  "Insert an info comment at point.

The comment is muli-line if possible.  It consists of 4 lines: 2 empty ones, one
inserted by 'insert-cm-dates' and one with a name (`init-info-comment-name').
After insertion, point is positioned at the beginning of the first line in the
comment."
  (interactive)
  (let ((comment-style 'multi-line)
        (start (point))
        (first-line nil))
    (insert (format "X\nX\n%s\n%s\n\n" (init-get-cm-dates)
                    init-info-comment-name))
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
    (goto-char first-line)))

(defun init-after-make-frame (new-frame)
  "Toggle fullscreen, disappear scrollbar."
  (select-frame new-frame)
  (init-toggle-fullscreen)
  (when (display-graphic-p)
    (scroll-bar-mode -1))
  ;; temporary fix for cursor color
  (set-cursor-color "red"))

(defun init-rm-old-backups (age)
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
     (message (concat "Removed %d old backup file" (if (eq count 1) "" "s"))
              count))))

(defvar-local init-linum-fmt-str "%01d"
  "Format string used by `init-linum-format'.")

(defun init-linum-before-numbering ()
  "Set `init-linum-fmt-str' to zero-padded format string."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (setq init-linum-fmt-str (concat "%0" (number-to-string w) "d"))))

(defun init-linum-format (line)
  "Format line using the format string `init-linum-fmt-str'."
  (propertize (format init-linum-fmt-str line) 'face 'linum))

;;;; Package customization

;;; fill-column-indicator
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "#253035")

;;; c-mode
(setq-default c-basic-offset 4)

;;; lua-mode
(setq lua-indent-level 2)

;;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode) t)

;;; linum-mode
(add-hook 'linum-before-numbering-hook 'init-linum-before-numbering t)
(setq linum-format 'init-linum-format)

;;;; Useful modes for programming mode hooks

;; Add all modes in 'modes' to all hooks in 'hooks'
(let ((modes
       '(fci-mode
         linum-mode
         hs-minor-mode))
      (hooks
       '(text-mode-hook
         c-mode-hook
         python-mode-hook
         emacs-lisp-mode-hook
         java-mode-hook
         autoconf-mode-hook
         sh-mode-hook
         lua-mode-hook
         jam-mode-hook
         c++-mode-hook
         nxml-mode-hook
         makefile-mode-hook
         sql-mode-hook)))
  (dolist (mode modes)
    (dolist (hook hooks)
      (add-hook hook mode t))))

;;;; Keybindings
;;;; `C-c [A-Za-z]' is reserved for users

;; window-resizing
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; misc
(global-set-key (kbd "C-c i") 'init-insert-info-comment)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c t") 'init-toggle-fullscreen)
(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-c x") 'ucs-insert)
(global-set-key (kbd "C-c g") 'global-auto-revert-mode)

;;;; Enabled commands

(put 'downcase-region 'disabled nil)

;;;; Themes

(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'dark-emacs t)

;; 80 columns should comfortably fit on small screens
(when (and (display-graphic-p) (< (display-pixel-width) 1400))
  (set-face-attribute 'default nil :height 90))

;;;; Hooks and similar

;; after a new frame is made
(add-hook 'after-make-frame-functions 'init-after-make-frame t)

;; before buffer is saved to file
(add-hook 'before-save-hook 'init-update-modification-date t)
(add-hook 'before-save-hook 'init-auto-convert-lineending t)
(setq require-final-newline t)

;;;; MISC

;; coding
(prefer-coding-system 'utf-8-unix)

;;; Backup
(setq backup-directory-alist `(("." . "~/.emacs.d/backup/files/"))
      backup-by-copying t
      version-control t
      kept-new-versions 2
      kept-old-versions 0
      delete-old-versions t)

;; remove backups older than 30 days
(init-rm-old-backups (days-to-time 30))

;; auto-save
(setq auto-save-list-file-prefix "~/.emacs.d/backup/auto-saves/saves-")

;; turn icomplete mode on
(icomplete-mode 1)

;; visual
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)

;; selections
(transient-mark-mode 1)
(delete-selection-mode 1)

;; visualize size of buffer
(size-indication-mode 1)

;; numbers of columns to fill
(setq-default fill-column 80)

;; line and column numbers
(line-number-mode -1)
(column-number-mode 1)

;; indentation
(setq-default indent-tabs-mode nil)

;; comments
(setq comment-multi-line t)

;; tab-completion
(setq tab-always-indent 'complete)
