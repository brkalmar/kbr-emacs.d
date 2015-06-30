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
(defvar-local bkalmar/packages/packages
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

(defun bkalmar/packages/check-install ()
  "Install each package in `bkalmar/packages/packages'."
  (let (to-install)
    (when
        (dolist (pkg bkalmar/packages/packages to-install)
          (when (not (package-installed-p pkg))
            (add-to-list 'to-install pkg t)))
      (package-refresh-contents)
      (mapc 'package-install to-install)
      (message "Installed %d new package%s" (length to-install)
               (if (eq (length to-install) 1) "" "s")))))

(defvar-local bkalmar/packages/checked-file
  "~/.emacs.d/packages/elpa/.last-checked"
  "Used by `bkalmar/packages/check-upgrade'.")

(defun bkalmar/packages/check-upgrade (age)
  "Call `package-list-packages' after user confirmation if
`bkalmar/packages/checked-file' contains a time older than AGE."
  (let ((filename (expand-file-name bkalmar/packages/checked-file)))
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

(bkalmar/packages/check-install)
(bkalmar/packages/check-upgrade (days-to-time 7))

;;;; Initialize manually installed packages.

(defvar bkalmar/packages/manual
  "~/.emacs.d/packages/manual"
  "Directory where manually installed packages are.")

;;; Package name
;; Version: YYYYMMDD
;; Updated: YYYY-MM-DD
;; Source: http://example.com/
;; Note: Something about build, etc...
;; (add-to-list 'load-path
;;              (concat (file-name-as-directory bkalmar/packages/manual)
;;                      "package-name-YYYYMMDD") t)

;;; Subtitles
;; Version: 1.100
;; Updated: 2015-05-04
;; Source: http://lists.gnu.org/archive/html/gnu-emacs-sources/2009-05/msg00007.html
;; Note: NONE
(add-to-list 'load-path (concat (file-name-as-directory bkalmar/packages/manual)
                                "subtitles-1.100") t)
