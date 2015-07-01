;; Custom startup file for GNU-emacs.
;;
;; 2014  Bence Kalmar

;;;; Packages

(load-file (concat user-emacs-directory "init-packages.el"))

;;;; System-specific

(cond
 ((equal system-type 'gnu/linux)
  (load-file (concat user-emacs-directory "gnu-linux/init.el")))
 ((equal system-type 'windows-nt)
  (load-file (concat user-emacs-directory "windows-nt/init.el")))
 (t
  (message "Could not find appropriate config file for system type: ‘%s’"
           system-type)))

;;;; Functions & variables

(defvar bkalmar/userdir nil
  "Real user directory, \"~\" in GNU/Linux, not necessarily \"~\" in Windows.

Should be set in OS-specific files.")

(defvar bkalmar/emacs-config-directory (concat user-emacs-directory ".config/")
  "Directory for all packages' config/history/etc. files.")

(defvar bkalmar/preferred-fonts-monospace nil
  "List of preferred monospace fonts, in descending order of preference.

Should be set in OS-specific files.")

(defvar bkalmar/preferred-fonts-proportional nil
  "List of preferred proportional fonts, in descending order of preference.

Should be set in OS-specific files.")

(defun bkalmar/random-bytes (n)
  "Get a string of N random bytes from `random'."
  (random t)
  (let ((res (make-string n 0)))
    (dotimes (i n res)
      (aset res i (random 256)))))

(defun bkalmar/random-string (n &optional chars)
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

(defvar bkalmar/copyright-comment-license-alist
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
`bkalmar/insert-copyright-comment'.")

; " <-- this double quote fixes a font-lock bug with the above strings

(defvar bkalmar/copyright-comment-name "Bence Kalmar"
  "The name used by `bkalmar/insert-copyright-comment'.")

(defvar bkalmar/copyright-comment-mail "bkalmar1996@gmail.com"
  "The e-mail used by `bkalmar/insert-copyright-comment'.")

(defvar bkalmar/copyright-comment-fmt "\


Copyright %s  %s <%s>

%s"
  "Format string to be used by `bkalmar/insert-copyright-comment'.

It must have 4 unescaped \"%s\" format specifiers, as follows: year, name,
e-mail, copyright text.")

(defun bkalmar/insert-copyright-comment (license)
  "Insert a copyright notice comment at point.

LICENSE must be a key in `bkalmar/copyright-comment-license-alist', the name of the
license description to insert.  Interactively, the user is asked to choose.

The comment adheres to <http://www.gnu.org/licenses/gpl-howto.html>.  Its style
is extra-line.  After insertion, point is positioned at the end of the first
line.

See also `bkalmar/copyright-comment-name', `bkalmar/copyright-comment-mail',
`bkalmar/copyright-comment-fmt'."
  (interactive
   (list
    (completing-read "License type: " bkalmar/copyright-comment-license-alist
                     nil t)))
  (let ((comment-style 'extra-line)
        (beg (point))
        (license-text
         (cdr (assoc license bkalmar/copyright-comment-license-alist)))
        (empty-pattern (bkalmar/random-string 50 "abcdefghijklmnopqrstuvwxyz"))
        end)
    (when (null license-text)
      (error "%S is not a key in `bkalmar/copyright-comment-license-alist'"
             license))
    (insert
     (replace-regexp-in-string
      "^$" empty-pattern
      (format bkalmar/copyright-comment-fmt
              (format-time-string "%Y") bkalmar/copyright-comment-name
              bkalmar/copyright-comment-mail license-text)))
    (comment-region beg (point))
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward (regexp-quote empty-pattern) end t)
      (replace-match "" t))
    (goto-char beg)
    (move-end-of-line nil)))

(defun bkalmar/visuals ()
  "Toggle fullscreen; disappear scrollbar."
  (interactive)
  (toggle-frame-maximized)
  (when (display-graphic-p)
    (scroll-bar-mode -1))
  ;; temporary fix for Debian
  (set-cursor-color "#ffcc00"))

(defun bkalmar/after-make-frame (new-frame)
  "Call `bkalmar/visuals'."
  (select-frame new-frame)
  (bkalmar/visuals))

(defun bkalmar/rm-old-backups (age)
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

(defvar bkalmar/linum-fmt-str "%01d"
  "Format string used by `bkalmar/linum-format'.")

(defun bkalmar/linum-before-numbering ()
  "Set `bkalmar/linum-fmt-str' to zero-padded format string."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (setq bkalmar/linum-fmt-str (concat "%0" (number-to-string w) "d"))))

(defun bkalmar/linum-format (line)
  "Format line using the format string `bkalmar/linum-fmt-str'."
  (propertize (format bkalmar/linum-fmt-str line) 'face 'linum))

(defun bkalmar/buffer-file-truename-last (n &optional prefix not-abs)
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

(defun bkalmar/choose-php-or-web-mode ()
  "Choose & call either `php-mode' or `web-mode' based on buffer contents."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      ;; pure-php scripts should not contain closing "?>"
      (if (search-forward "?>" nil t)
          (web-mode)
        (php-mode)))))

(defun bkalmar/temp-buffer ()
  "Create & switch to new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "%temp%")))

(defun bkalmar/find-org-default-notes-file ()
  "Call `find-file' on `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

(defvar bkalmar/disable-themes-list ()
  "The themes that have been last disabled by `bkalmar/disable-themes', if any.")

(defun bkalmar/disable-themes ()
  "Disable all themes."
  (interactive)
  (message "Disabling all themes...")
  (setq bkalmar/disable-themes-list (mapc 'disable-theme custom-enabled-themes)))

(defun bkalmar/enable-themes ()
  "Enable all themes that have been disabled.
If no themes have been disabled, do nothing."
  (interactive)
  (if bkalmar/disable-themes-list
      (progn
        (message "Enabling disabled themes...")
        (mapc 'enable-theme bkalmar/disable-themes-list))
    (message "No disabled themes to enable.")))

(defvar-local bkalmar/wordprocessor-fringe-bg nil
  "Original background for face `fringe', modified by `bkalmar/wordprocessor-mode',
if any.")

(define-minor-mode bkalmar/wordprocessor-mode
  "Minor mode for a \"word processor look\" in the current window.  Uses the
default theme and some margins on both sides."
  nil " WP" nil
  (if bkalmar/wordprocessor-mode
      (progn
        ;; disable linum mode
        (linum-mode -1)
        ;; wordwrap
        (visual-line-mode +1)
        ;; adjust font size
        (text-scale-adjust +1.5)
        ;; disable themes
        (bkalmar/disable-themes)
        ;; margins
        (setq left-margin-width 20)
        (setq right-margin-width 20)
        ;; invisible fringes
        (setq bkalmar/wordprocessor-fringe-bg (face-attribute 'fringe :background))
        (set-face-attribute 'fringe nil :background
                            (face-attribute 'default :background))
        ;; update window
        (set-window-buffer nil (current-buffer)))
    ;; enable linum mode
    (linum-mode +1)
    ;; no wordwrap
    (visual-line-mode -1)
    ;; reset font size
    (text-scale-adjust -1.5)
    ;; visible fringes
    (set-face-attribute 'fringe nil :background bkalmar/wordprocessor-fringe-bg)
    ;; enable themes
    (bkalmar/enable-themes)
    ;; margins
    (setq left-margin-width 0)
    (setq right-margin-width 0)
    ;; update window
    (set-window-buffer nil (current-buffer))))

(defun bkalmar/sort-lines-random (beg end)
  "Sort lines in region randomly, using `random' as a source of randomness."
  (interactive "r\n")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (a b) (eq (random 2) 1)))))))

;; source: https://gist.github.com/haxney/3055728
(defun bkalmar/font-is-mono-p (font-family)
  "Return `t' if `font-family' appears to be a monospace font's family, `nil'
otherwise."
  (let (width)
    (with-temp-buffer
      (set-window-buffer nil (current-buffer))
      (text-scale-set 4)
      (insert (propertize "a b c d l" 'face `(:family ,font-family)))
      (goto-char (line-end-position))
      (setq width (car (posn-x-y (posn-at-point))))
      (newline)
      (insert (propertize "v w x y m" 'face `(:family ,font-family)))
      (goto-char (line-end-position))
      (eq width (car (posn-x-y (posn-at-point)))))))

(defvar bkalmar/compare-fonts-history '()
  "History list for `bkalmar/compare-fonts'.")

(defvar bkalmar/compare-fonts-invalid-char ?�
  "Used to replace those characters in the test string — after insertion — which
have no font / are displayed in an overloaded font.")

(defvar bkalmar/compare-fonts-test-text
  ;; source: http://www.columbia.edu/~fdc/utf8/index.html#glass
  "The quick brown fox jumps over the lazy dog. 1 l I i 0 O o.  Tudok üveget enni anélkül hogy bajom lenne tőle.  Aš galiu valgyti stiklą ir jis manęs nežeidžia.  Μπορώ να φάω σπασμένα γυαλιά χωρίς να πάθω τίποτα.  Можам да јадам стакло, а не ме штета.  我能吞下玻璃而不傷身體。  나는 유리를 먹을 수 있어요. 그래도 아프지 않아요.  ∀∁∅∋∉ℕ∑∜∞∧∩∪∟∠∭∰∴≅≈≣≤≫≹⊃⊭⋅⋘ ℃Ω ⏚⏦  .من می توانم بدونِ احساس درد شيشه بخورم  ﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥  .איך קען עסן גלאָז און עס טוט מיר נישט װײ  ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ"
  "Default test text for `bkalmar/compare-fonts'.  Normally should not have a
  closing newline.")

;; source: https://gist.github.com/haxney/3055728
(defun bkalmar/compare-fonts (&optional test-text font-type only-regular-text)
  "Display a list of font faces with some test text for each.

If `test-text' is `nil', use `bkalmar/compare-fonts-test-text'.  Interactively,
with a single C-u prefix argument ask for a custom test string, use
`bkalmar/compare-fonts-test-text' otherwise.

If `font-type' is `nil', display all fonts; otherwise it must be one of
`:monospace' or `:proportional' to display only those type of fonts.
Interactively, ask what type of fonts to display.

If `only-regular-text' is non-`nil', display only the regularly formatted test
text, not the bold or italic ones for each font.  Interactively, ask whether to
display only regular text."
  (interactive
   (let (test-text font-type only-regular-text)
     (when (equal current-prefix-arg '(4))
       (setq test-text
             (read-string "Test text for each font: " nil
                          'bkalmar/compare-fonts-history
                          bkalmar/compare-fonts-test-text)))
     (setq font-type
           (cdr (assoc
                 (completing-read "Type of fonts to display (default all): "
                                  '("monospace" "proportional" "all") nil t nil
                                  'bkalmar/compare-fonts-history "all")
                 '(("all" . :all)
                   ("monospace" . :monospace)
                   ("proportional" . :proportional)))))
     (setq only-regular-text
           (not (y-or-n-p "Show bold and italic test text? ")))
     (list test-text font-type only-regular-text)))
  (when (null test-text)
    (setq test-text bkalmar/compare-fonts-test-text))
  (when (null font-type)
    (setq font-type :all))
  (let ((count 0)
        ;; predicate for whether to display a font family; based on `font-type'
        (display-font-p
         (cond ((eq font-type :all) (lambda (x) t))
               ((eq font-type :monospace) 'bkalmar/font-is-mono-p)
               ((eq font-type :proportional)
                (lambda (x) (not (bkalmar/font-is-mono-p x))))
               (t (error "Unrecognized `font-type' argument"))))
        pos-text-beg
        pos-text-end)
    (pop-to-buffer "*Fonts*")
    (erase-buffer)
    (dolist (font-family (sort (delete-dups (font-family-list)) 'string<))
      (when (funcall display-font-p font-family)
        (setq count (1+ count))
        ;; insert font's name
        (insert (format "%s:\n" (propertize font-family 'face '(:weight bold))))
        (setq pos-text-beg (point))
        ;; insert test text
        (insert
         (propertize (format "%s\n" test-text) 'face `(:family ,font-family)))
        (when (not only-regular-text)
          (insert
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :slant italic))
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :weight bold))
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :slant italic
                                       :weight bold))))
        (newline)
        ;; replace fontless/overloaded characters in inserted test text
        (setq pos-text-end (point))
        (goto-char pos-text-beg)
        (while (not (= (point) pos-text-end))
          (if (and
               ;; ignore newlines
               (not (= (char-after) ?\n))
               (or
                ;; character cannot be displayed
                (null (font-at (point)))
                ;; character is displayed in font other than desired (overloaded
                ;; font)
                (not (eq (font-get (font-at (point)) :family)
                         (intern font-family)))))
              (progn
                (delete-char 1)
                (insert bkalmar/compare-fonts-invalid-char))
            (forward-char)))))
    ;; insert number of fonts displayed
    (goto-char (point-min))
    (insert (format "%d fonts\n\n" count))))

(defun bkalmar/insert-header-guard ()
  "Insert a C/C++ ifndef/endif header guard at the beginning and end of the
  current buffer.  Use a preprocessor identifier based on the buffer's filename,
  making sure it is unique by appending random letters."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((preprocessor-id
             (concat
              (if (buffer-file-name)
                  ;; convert to all-uppercase
                  (upcase
                   ;; remove more than 1 consecutive underscores
                   (replace-regexp-in-string
                    "\\(_\\)\\(_+\\)" ""
                    ;; strip preceding & trailing underscores
                    (replace-regexp-in-string
                     "\\(^_+\\|_+$\\)" ""
                     ;; remove non-letter or -undescore characters
                     (replace-regexp-in-string
                      "[^a-zA-Z_]" ""
                      ;; replace dots and hyphens by underscores
                      (replace-regexp-in-string
                       "[.-]+" "_"
                       ;; take the filename
                       (file-name-nondirectory (buffer-file-name))
                       t t) t t) t t) t t 1))
                (error "this buffer has no filename"))
              "_" (bkalmar/random-string 10 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
        (goto-char (point-min))
        (insert (format "#ifndef %s\n#define %s\n\n"
                        preprocessor-id preprocessor-id))
        (goto-char (point-max))
        (when (not (= (char-before) ?\n))
          (insert ?\n))
        (insert (format "\n#endif /* %s */\n" preprocessor-id))))))

;;;; Packages & modes customization

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
(add-hook 'linum-before-numbering-hook 'bkalmar/linum-before-numbering t)
(setq linum-format 'bkalmar/linum-format)

;;; auto-complete-mode
(global-auto-complete-mode t)
(customize-set-variable
 'ac-comphist-file
 (expand-file-name
  (concat bkalmar/emacs-config-directory "auto-complete/ac-comphist.dat")))
(mkdir (file-name-directory ac-comphist-file) t)

;;; web-mode & php-mode
(add-to-list 'auto-mode-alist '("\\.php$" . bkalmar/choose-php-or-web-mode) t)

;;; php-mode
(setq php-template-compatibility nil)

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
(setq org-log-repeat nil)
(setq org-log-into-drawer "LOGBOOK")

;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STAR(s)" "|" "DONE(d!)")
        (sequence "CONS(k)" "|")
        (sequence "|" "CANC(c!)")))
(setq org-enforce-todo-dependencies t)

;; deadlines / scheduling
; no warning
(setq org-deadline-warning-days 0)

;; tags
(setq org-tags-column -80)

;; properties
(setq org-use-property-inheritance t)

;; date / time
(setq org-time-stamp-custom-formats
      '("<%Y-%m-%d %V|%a>" . "<%Y-%m-%d %V|%a %H:%M>"))
(setq-default org-display-custom-times t)

;; capture
(setq org-directory (concat bkalmar/userdir "/documents"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

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

;;; calendar
(setq calendar-date-style 'iso)

;; start week with Monday
(setq calendar-week-start-day 1)

;; ISO week numbers
(defface bkalmar/calendar-iso-week
  '((t . (:height 0.9 :inherit font-lock-builtin-face)))
  "Face used to highlight ISO week numbers in calendar.")

(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car (calendar-iso-from-absolute
                           (calendar-absolute-from-gregorian
                            (list month day year)))))
        'font-lock-face 'bkalmar/calendar-iso-week))

;;; Subtitles
(require 'subtitles)

;;; Ido
(customize-set-variable 'ido-save-directory-list-file
                        (concat bkalmar/emacs-config-directory "ido/ido.last"))
(mkdir (file-name-directory ido-save-directory-list-file) t)

;;; bookmark
(customize-set-variable
 'bookmark-default-file
 (concat bkalmar/emacs-config-directory "bookmark/bookmarks"))
(mkdir (file-name-directory bookmark-default-file) t)

;;; idlwave
(customize-set-variable 'idlwave-config-directory
                        (concat bkalmar/emacs-config-directory "idlwave/"))

;;; url
(require 'url)
(customize-set-variable 'url-configuration-directory
                        (concat bkalmar/emacs-config-directory "url/"))

;;; x-win
(require 'x-win)
(defun emacs-session-filename (session-id)
  (let ((dir (concat bkalmar/emacs-config-directory "x-win/")))
    (mkdir dir t)   
    (expand-file-name (concat dir "session." session-id))))

;;;; Useful modes for prose-like mode hooks

(add-hook 'bibtex-mode-hook 'variable-pitch-mode t)
(add-hook 'help-mode-hook 'variable-pitch-mode t)
(add-hook 'Info-mode-hook 'variable-pitch-mode t)
(add-hook 'LaTeX-mode-hook 'variable-pitch-mode t)
(add-hook 'markdown-mode-hook 'variable-pitch-mode t)

;;;; Useful modes for programming mode hooks

;; NOTE: fci-mode temporarily removed because of incompatibility with
;; auto-complete & web-mode
(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'text-mode-hook 'linum-mode t)

(add-hook 'prog-mode-hook 'form-feed-mode t)
(add-hook 'text-mode-hook 'form-feed-mode t)

;; (add-hook 'prog-mode-hook 'hs-minor-mode t)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(add-hook 'prog-mode-hook 'electric-indent-mode)

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
(global-set-key (kbd "C-c n") 'bkalmar/find-org-default-notes-file)
;; `C-c ,` as described in the manual doesn't work
(global-set-key (kbd "C-c p") 'org-priority)
(global-set-key (kbd "C-c u") 'org-capture)

;;; misc
(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)
(global-set-key (kbd "C-c f") 'toggle-frame-maximized)
(global-set-key (kbd "C-c h") 'bkalmar/enable-themes)
(global-set-key (kbd "C-c H") 'bkalmar/disable-themes)
(global-set-key (kbd "C-c i") 'bkalmar/insert-copyright-comment)
(global-set-key (kbd "C-c l") 'fill-region)
(global-set-key (kbd "C-c m") 'normal-mode)
(global-set-key (kbd "C-c L") 'fill-region-as-paragraph)
;; q logically corresponds to C-q `quoted-insert'
(global-set-key (kbd "C-c q") 'insert-char)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)
(global-set-key (kbd "C-c t") 'bkalmar/temp-buffer)
(global-set-key (kbd "C-c v") 'global-auto-revert-mode)
(global-set-key (kbd "C-c w") 'bkalmar/wordprocessor-mode)

;;;; Enabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; Themes

(setq custom-theme-directory (concat user-emacs-directory "themes"))
(load-theme 'zenburn t)
(load-theme 'bkalmar t)

;; default & fixed-pitch font
(catch 'break
  (dolist (font-family bkalmar/preferred-fonts-monospace)
    (when (member font-family (font-family-list))
      (set-face-attribute 'default nil :family font-family)
      (set-face-attribute 'fixed-pitch nil :family font-family)
      (throw 'break t))))

;; variable-pitch font
(catch 'break
  (dolist (font-family bkalmar/preferred-fonts-proportional nil)
    (when (member font-family (font-family-list))
      (set-face-attribute 'variable-pitch nil :family font-family)
      (throw 'break t))))

;; 80 columns should comfortably fit on small screens
(when (and (display-graphic-p) (< (display-pixel-width) 1400))
  (set-face-attribute 'default nil :height 90))

;;;; Hooks and similar

;; after a new frame is made
(add-hook 'after-make-frame-functions 'bkalmar/after-make-frame t)

;; before buffer is saved to file
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
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/files/")))
      backup-by-copying t
      version-control t
      kept-new-versions 2
      kept-old-versions 0
      delete-old-versions t)

;; remove backups older than 30 days
(bkalmar/rm-old-backups (days-to-time 30))

;; auto-save
(setq auto-save-list-file-prefix (concat user-emacs-directory "backup/auto-saves/saves-"))

;; turn icomplete mode on
(icomplete-mode 1)

;;; Visual

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(bkalmar/visuals)

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
      '((:eval (or (bkalmar/buffer-file-truename-last 2 "•••/" t) "%b")) " (%I)"))
(setq icon-title-format "%b")

;; frame resize
(setq frame-resize-pixelwise t)

;;; Locale

(setq system-time-locale "C")
