;; Custom startup file for GNU-emacs.
;;
;; 2013-05-03 / 2014-01-13
;; AlbusDrachir

;;;; Packages

(load-file "~/.emacs.d/init-packages.el")

;;;; Functions

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

(defun get-cm-dates ()
  "Return the creation and modification date (today) as a string in format
'YYYY-MM-DD / YYYY-MM-DD'."
  (let (
        (date (format-time-string "%Y-%m-%d")))
    (concat date " / " date))
  )

(setq info-name "Bence Kalmar")

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
  "Toggle fullscreen, disappear scrollbar."
  (select-frame new-frame)
  ;; (set-custom-colors)
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
