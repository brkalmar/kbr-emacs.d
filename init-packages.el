;; Add additional package archives.
;; Check if neccessary packages are installed, install them if they aren't.
;; Upgrade old packages.
;;
;; Bence Kalmar

;;; Necessary to actually initialize the package system.
(require 'package)

;;; Archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;; Initialize all ELPA packages.

(setq package-user-dir "~/.emacs.d/packages/elpa")
(setq package-enable-at-startup nil)
(setq package-load-list '(all))
(package-initialize)

;;; Packages
(defvar-local init-packages-packages
  '(;; gnu
    auctex
    nhexl-mode
    ;; melpa
    auto-complete
    auto-complete-auctex
    fill-column-indicator
    git-commit-mode
    haskell-mode
    json-mode
    lua-mode
    markdown-mode
    org
    palette
    php-mode
    rainbow-delimiters
    scala-mode2
    tt-mode
    web-mode
    yaml-mode
    zenburn-theme)
  "Check if these packages are installed at startup.")

(defun init-packages-check-install ()
  "Install each package in `init-packages-packages'."
  (let (to-install)
    (when
        (dolist (pkg init-packages-packages to-install)
          (when (not (package-installed-p pkg))
            (add-to-list 'to-install pkg t)))
      (package-refresh-contents)
      (mapc 'package-install to-install)
      (message "Installed %d new package%s" (length to-install)
               (if (eq (length to-install) 1) "" "s")))))

(defvar-local init-packages-checked-file
  "~/.emacs.d/packages/elpa/.last-checked"
  "Used by `init-packages-check-upgrade'.")

(defun init-packages-check-upgrade (age)
  "Call `package-list-packages' after user confirmation if
`init-packages-checked-file' contains a time older than AGE."
  (let ((filename (expand-file-name init-packages-checked-file)))
    (with-temp-buffer
      (when (or (not (file-exists-p filename))
                (progn (insert-file-contents-literally filename)
                       (time-less-p (date-to-time (buffer-string))
                                    (time-subtract (current-time) age))))
        (delete-region (point-min) (point-max))
        (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
        (write-file filename)
        (if (y-or-n-p-with-timeout
             "Check for upgradable packages? [y in 10 seconds] " 10 t)
            (package-list-packages))))))

(init-packages-check-install)
(init-packages-check-upgrade (days-to-time 7))

;;;; Initialize manually installed packages.

(defvar init-packages-manual
  "~/.emacs.d/packages/manual"
  "Directory where manually installed packages are.")

;;; Package name
;; Version: YYYYMMDD
;; Updated: YYYY-MM-DD
;; Source: http://example.com/
;; Note: Something about build, etc...
;; (add-to-list 'load-path (concat (file-name-as-directory init-packages-manual)
;;                                 "package-name-YYYYMMDD") t)
