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

(defun bkalmar/find-org-default-notes-file ()
  "Call `find-file' on `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

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
