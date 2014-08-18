;; Custom startup file for GNU-emacs.
;;
;; 2014  Bence Kalmar

;;;; Packages

;; IMPORTANT: Must place this *before* any CEDET component (including EIEIO)
;; gets activated by another package (Gnus, auth-source, ...).
(load-file "~/.emacs.d/packages/manual/cedet-20140410/cedet-devel-load.el")

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

(defvar init-userdir nil
  "Real user directory, \"~\" in GNU/Linux, not necessarily \"~\" in Windows.

Should be set in OS-specific files.")

(defun init-random-bytes (n)
  "Get a string of N random bytes from `random'."
  (random t)
  (let ((res (make-string n 0)))
    (dotimes (i n res)
      (aset res i (random 256)))))

(defun init-random-string (n &optional chars)
  "Get a string of N random characters from `random'.

If CHARS is non-nil, it must be a string containing characters to choose from.
Otherwise choose from all characters in the allowable range."
  (random t)
  (let ((res (make-string n 0)))
    (dotimes (i n res)
      (aset
       res i
       (if (null chars)
           (random (1+ (max-char)))
         (aref chars (random (length chars))))))))

(defvar init-auto-convert-lineending-skip-list
  '("~/.emacs.d/url/cookies"
    "~/.emacs.d/packages/elpa/archives/gnu/archive-contents"
    "~/.emacs.d/packages/elpa/archives/marmalade/archive-contents"
    "~/.emacs.d/packages/elpa/archives/melpa/archive-contents"
    "~/.emacs.d/packages/elpa/.last-refresh")
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

(defvar init-copyright-comment-license-alist
  '(("gpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")
    ("lgpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")
   ("agpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>."))
  "GNU license names and their associated copyright text used by
`init-insert-copyright-comment'.")

; " <-- this double quote fixes a font-lock bug with the above strings

(defvar init-copyright-comment-name "Bence Kalmar"
  "The name used by `init-insert-copyright'.")

(defvar init-copyright-comment-mail "bkalmar1996@gmail.com"
  "The e-mail used by `init-insert-copyright'.")

(defvar init-copyright-comment-fmt "\


Copyright %s  %s <%s>

%s"
  "Format string to be used by `init-insert-copyright-comment'.

It must have 4 unescaped \"%s\" format specifiers, as follows: year, name,
e-mail, copyright text.")

(defun init-insert-copyright-comment (license)
  "Insert a copyright notice comment at point.

LICENSE must be a key in `init-copyright-comment-license-alist', the name of the
license description to insert.  Interactively, the user is asked to choose.

The comment adheres to <http://www.gnu.org/licenses/gpl-howto.html>.  Its style
is extra-line.  After insertion, point is positioned at the end of the first
line.

See also `init-copyright-comment-name', `init-copyright-comment-mail',
`init-copyright-comment-fmt'."
  (interactive
   (list
    (completing-read "License type: " init-copyright-comment-license-alist nil t)))
  (let ((comment-style 'extra-line)
        (beg (point))
        (license-text (cdr (assoc license init-copyright-comment-license-alist)))
        (empty-pattern (init-random-string 50 "abcdefghijklmnopqrstuvwxyz"))
        end)
    (when (null license-text)
      (error "%S is not a key in `init-copyright-comment-license-alist'" license))
    (insert
     (replace-regexp-in-string
      "^$" empty-pattern
      (format init-copyright-comment-fmt
              (format-time-string "%Y") init-copyright-comment-name
              init-copyright-comment-mail license-text)))
    (comment-region beg (point))
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward (regexp-quote empty-pattern) end t)
      (replace-match "" t))
    (goto-char beg)
    (move-end-of-line nil)))

(defun init-visuals ()
  "Toggle fullscreen; disappear scrollbar."
  (interactive)
  (init-toggle-fullscreen)
  (when (display-graphic-p)
    (scroll-bar-mode -1))
  ;; temporary fix for Debian
  (set-cursor-color "#ffcc00"))

(defun init-after-make-frame (new-frame)
  "Call `init-visuals'."
  (select-frame new-frame)
  (init-visuals))

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

(defun init-buffer-file-truename-last (n &optional prefix not-abs)
  "Return string of last N path elements of `buffer-file-truename' or nil if
`buffer-file-truename' is nil.

If PREFIX is non-nil, prefix the returned string with it.

If NOT-ABS is non-nil, do not prefix the string if it's an absolute path."
  (when buffer-file-truename
    (let ((elems (split-string buffer-file-truename "/"))
           elems-last
           res
           pre
           post
           is-full)
      ;; starts with a "/"
      (when (eq (nth 0 elems) "")
        (setq pre "/")
        (pop elems))
      ;; ends with a "/"
      (when (eq (car (last elems)) "")
        (setq post "/")
        (nbutlast elems))
      (setq elems-last (last elems n))
      (setq is-full (= (length elems) (length elems-last)))
      (setq res (concat (if is-full pre) (mapconcat 'identity elems-last "/") post))
      (if (and not-abs is-full)
          res
        (concat prefix res)))))

(defun init-choose-php-or-web-mode ()
  "Choose & call either `php-mode' or `web-mode' based on buffer contents."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      ;; pure-php scripts should not contain closing "?>"
      (if (search-forward "?>" nil t)
          (web-mode)
        (php-mode)))))

(defun init-temp-buffer ()
  "Create & switch to new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "%temp%")))

(defun init-find-org-default-notes-file ()
  "Call `find-file' on `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

;;;; Package customization

;;; diary
(require 'diary-lib)

;;; fill-column-indicator
(setq fci-rule-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "#253035")

;;; c-mode
(setq-default c-basic-offset 4)

;;; emacs-lisp-mode
;; use `fill-column's value
(setq emacs-lisp-docstring-fill-column nil)

;;; lua-mode
(setq lua-indent-level 2)

;;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode) t)

;;; linum-mode
(add-hook 'linum-before-numbering-hook 'init-linum-before-numbering t)
(setq linum-format 'init-linum-format)

;;; auto-complete-mode
(global-auto-complete-mode t)

;;; web-mode & php-mode
(add-to-list 'auto-mode-alist '("\\.php$" . init-choose-php-or-web-mode) t)

;;; php-mode
(setq php-template-compatibility nil)

;;; semantic mode
(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-mru-bookmark-mode
        global-semantic-higlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-decoration-mode
        global-semantic-idle-local-symbol-highlight-code
        global-semantic-idle-scheduler-mode
        global-semantic-idle-completions-mode
        global-semantic-idle-summary-mode))

(semantic-mode 1)

;;; srecode
(global-srecode-minor-mode 1)

;;; python-mode
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq py-load-pymacs-p t)

;; docstrings & comments
(setq py-docstring-fill-column nil)
(setq py-comment-fill-column nil)
(setq py-docstring-style 'symmetric)
(setq-default py-fill-paragraph t)

;;; dired
(setq dired-auto-revert-buffer t)

;;; palette
(setq palette-font "-misc-fixed-medium-r-normal--6-*-75-75-c-40-iso8859-1")

;;; org
(require 'org)
(setq org-startup-folded 'content)
(setq org-startup-indented t)

;; invisible edits
(setq org-catch-invisible-edits 'show)

;; link abbreviations
(add-to-list 'org-link-abbrev-alist '("github" . "https://github.com/%s/"))
(add-to-list 'org-link-abbrev-alist
             '("bitbucket" . "https://bitbucket.org/%s/"))
(add-to-list 'org-link-abbrev-alist
             '("melpa" . "http://melpa.milkbox.net/#/%s"))

;; logging
(setq org-log-repeat 'time)
; no "- State XYZ from ABC" line
(setcdr (assq 'state org-log-note-headings) "")

;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
        (sequence "CONSIDER(k)" "|")
        (sequence "|" "CANCELED(c)")))
(setq org-enforce-todo-dependencies t)

;; tags
(setq org-tags-column -80)

;; properties
(setq org-use-property-inheritance t)

;; date / time
(setq org-time-stamp-custom-formats
      '("<%Y-%m-%d %V|%a>" . "<%Y-%m-%d %V|%a %H:%M>"))
(setq-default org-display-custom-times t)

;; capture
(setq org-directory (concat init-userdir "/sync/documents"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(add-to-list 'init-auto-convert-lineending-skip-list org-default-notes-file)

;; archive
(setq org-archive-location "%s.archive.org::")

;; agenda
(add-to-list 'org-agenda-files org-default-notes-file)
(setq org-agenda-span 14
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-1d"
      org-agenda-use-time-grid nil)

;; refile
(add-to-list 'org-refile-targets '(org-agenda-files . (:maxlevel . 30)))

;;; haskell-mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;;;; Useful modes for programming mode hooks

;; NOTE: fci-mode temporarily removed because of incompatibility with
;; auto-complete & web-mode
(add-hook 'prog-mode-hook 'linum-mode t)

;; (add-hook 'prog-mode-hook 'hs-minor-mode t)

(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)) t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; custom auto-complete sources
(dolist (elem '(c-mode-hook
                c++-mode-hook
                python-mode-hook))
  (add-hook
   elem
   (lambda () (setq ac-sources (append ac-sources '(ac-source-semantic))))))

;; white-space sensitive languages
(dolist (elem '(python-mode-hook))
  (add-hook
   elem
   (lambda () (add-to-list
               'write-contents-functions 'delete-trailing-whitespace t))))

;;;; Keybindings
;;;; `C-c [A-Za-z]' is reserved for users

;;; overwrite
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; semantic mode
(global-set-key (kbd "C-c d") 'semantic-ia-show-doc)
(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c o") 'senator-fold-tag)
(global-set-key (kbd "C-c O") 'senator-unfold-tag)

;;; window-resizing
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;;; org
(global-set-key (kbd "C-c g") 'org-agenda)
(global-set-key (kbd "C-c n") 'init-find-org-default-notes-file)
;; `C-c ,` as described in the manual doesn't work
(global-set-key (kbd "C-c p") 'org-priority)
(global-set-key (kbd "C-c u") 'org-capture)

;;; misc
(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)
(global-set-key (kbd "C-c f") 'init-toggle-fullscreen)
(global-set-key (kbd "C-c i") 'init-insert-copyright-comment)
(global-set-key (kbd "C-c l") 'fill-region)
(global-set-key (kbd "C-c L") 'fill-region-as-paragraph)
;; q logically corresponds to C-q `quoted-insert'
(global-set-key (kbd "C-c q") 'insert-char)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)
(global-set-key (kbd "C-c t") 'init-temp-buffer)
(global-set-key (kbd "C-c v") 'global-auto-revert-mode)

;;;; Enabled commands

(put 'downcase-region 'disabled nil)

;;;; Themes

(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'bkalmar t)

;; 80 columns should comfortably fit on small screens
(when (and (display-graphic-p) (< (display-pixel-width) 1400))
  (set-face-attribute 'default nil :height 90))

;;;; Hooks and similar

;; after a new frame is made
(add-hook 'after-make-frame-functions 'init-after-make-frame t)

;; before buffer is saved to file
(add-hook 'before-save-hook 'init-auto-convert-lineending t)
(setq require-final-newline t)

;;;; MISC

;; coding
(prefer-coding-system 'utf-8)

;; quoted-insert base 10
(setq read-quoted-char-radix 10)

;; indentation
(setq-default indent-tabs-mode nil)

;; comments
(setq comment-multi-line t)

;; ido
(ido-mode 1)

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

;;; Visual

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(init-visuals)

;; half-width fringes
(fringe-mode 4)

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

;; frame & icon titles
(setq frame-title-format
      '((:eval (or (init-buffer-file-truename-last 2 "•••/" t) "%b")) " (%I)"))
(setq icon-title-format "%b")

;;; Locale

(setq system-time-locale "C")
