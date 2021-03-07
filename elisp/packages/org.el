(require 'org)
(require 'org-agenda)

(customize-set-variable 'org-startup-folded 'content)
(customize-set-variable 'org-startup-indented t)

;; invisible edits
(customize-set-variable 'org-catch-invisible-edits 'show)

;; link abbreviations
(add-to-list 'org-link-abbrev-alist '("github" . "https://github.com/%s/"))
(add-to-list 'org-link-abbrev-alist
             '("bitbucket" . "https://bitbucket.org/%s/"))
(add-to-list 'org-link-abbrev-alist
             '("melpa" . "http://melpa.milkbox.net/#/%s"))

;; logging
(customize-set-variable 'org-log-repeat nil)
(customize-set-variable ' org-log-into-drawer "LOGBOOK")

;; todo states
(customize-set-variable 'org-todo-keywords
                        '((sequence "TODO(t)" "STAR(s)" "|" "DONE(d!)")
                          (sequence "CONS(k)" "|")
                          (sequence "|" "CANC(c!)")))
(customize-set-variable 'org-enforce-todo-dependencies t)

;; deadlines / scheduling
; no warning
(customize-set-variable 'org-deadline-warning-days 0)

;; tags
(customize-set-variable 'org-tags-column -80)

;; properties
(customize-set-variable 'org-use-property-inheritance t)

;; date / time
(customize-set-variable 'org-time-stamp-custom-formats
                        '("<%Y-%m-%d %V|%a>" . "<%Y-%m-%d %V|%a %H:%M>"))
(customize-set-variable 'org-display-custom-times t)

;; capture
(customize-set-variable 'org-directory
                        (concat kbr/home-directory "/documents"))
(customize-set-variable 'org-default-notes-file
                        (concat org-directory "/notes.org"))

(defun kbr/find-org-default-notes-file ()
  "Call `find-file' on `org-default-notes-file'."
  (interactive)
  (find-file org-default-notes-file))

;; archive
(customize-set-variable 'org-archive-location "%s.archive.org::")

;; agenda
(add-to-list 'org-agenda-files org-default-notes-file)
(customize-set-variable 'org-agenda-span 14)
(customize-set-variable 'org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-1d")
(customize-set-variable 'org-agenda-use-time-grid nil)

;; refile
(add-to-list 'org-refile-targets '(org-agenda-files . (:maxlevel . 30)))
