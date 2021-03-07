;; Backup & autosave.
;;
;; 2015  Bence Kalmar

(customize-set-variable
 'backup-directory-alist
 `(("." . ,(concat bkalmar/emacs-directory "backup/files/"))))
(customize-set-variable 'backup-by-copying t)
(customize-set-variable 'version-control t)
(customize-set-variable 'kept-new-versions 2)
(customize-set-variable 'kept-old-versions 0)
(customize-set-variable 'delete-old-versions t)

(customize-set-variable
 'auto-save-list-file-prefix
 (concat bkalmar/emacs-directory "backup/auto-saves/saves-"))

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

(bkalmar/rm-old-backups (days-to-time 30))
