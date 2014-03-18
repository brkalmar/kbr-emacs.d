;; Add additional package archives.
;; Check if neccessary packages are installed, install them if they aren't.
;; Upgrade old packages.
;; 
;; Bence Kalmar

;;; Necessary to actually initialize the package system.
(require 'package)

;;; Archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;;; Initialize all ELPA packages.

(setq package-user-dir "~/.emacs.d/packages/elpa")
(setq package-enable-at-startup nil)
(setq package-load-list '(all))
(package-initialize)

;;; Packages
(setq package-archive-enable-alist
      ;; package-filter should be the very first package
      '(("melpa" package-filter json-mode fill-column-indicator git-commit-mode
         markdown-mode web-mode lua-mode)
        ("marmalade" nhexl-mode jam-mode)))

(defun init-packages-refresh-archives (age)
  "Refresh archives & mark in file as refreshed if they haven't been refreshed
in more than AGE time."
  (let ((filename (expand-file-name "~/.emacs.d/packages/elpa/.last-refresh"))
        (old-before (time-subtract (current-time) age))
        (exists t)
        contents)
    (with-temp-buffer
      (if (file-exists-p filename)
          (progn
            (insert-file-contents-literally filename)
            (setq contents
                  (buffer-substring-no-properties (point-min) (point-max))))
        (setq exists nil))
      (when (or (not exists) (time-less-p (date-to-time contents) old-before))
        (package-refresh-contents)
        (setq contents (format-time-string "%Y-%m-%dT%H:%M:%S")))
      (delete-region (point-min) (point-max))
      (insert contents)
      (write-file filename))))

(defun init-packages-check ()
  "Install or upgrade each package in each cdr in
`package-archive-enable-alist'."
  (init-packages-refresh-archives (days-to-time 7))
  (let (to-install
        (wait-time 5))
    (dolist (elt package-archive-enable-alist)
      (dolist (pkg (cdr elt))
        (when (not (package-installed-p pkg))
          (add-to-list 'to-install pkg t))))
    (dolist (pkg (package-menu--find-upgrades))
      (add-to-list 'to-install pkg t))
    (when (and to-install
               (y-or-n-p-with-timeout
                (format (concat "The following packages will be installed "
                                "or upgraded: %s. Proceed? (y in %d s) ")
                        (mapconcat 'symbol-name to-install ", ") wait-time)
                wait-time t))
      (mapc 'package-install to-install))))

(init-packages-check)

;;;; Initialize manually installed packages.

(defvar init-packages-manual
  "~/.emacs.d/packages/manual"
  "Directory where manually installed packages are.")

;;; Package name
;; Version: YYYYMMDD
;; Updated: YYYY-MM-DD
;; Source: http://example.com/
;; (add-to-list 'load-path (concat (file-name-as-directory init-packages-manual)
;;                                 "package-name-YYYYMMDD") t)
