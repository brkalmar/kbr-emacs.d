;; Add additional package archives.
;; Check if neccessary packages are installed, install them if they aren't.
;; Upgrade old packages.
;;
;; Bence Kalmar

(require 'package)

;;; config

(customize-set-variable 'package-user-dir
                        (concat user-emacs-directory "packages/elpa/"))
(customize-set-variable 'package-enable-at-startup nil)
(customize-set-variable 'package-load-list '(all))

;; initialize already installed packages
(package-initialize)

;;; ELPA packages

;; archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(customize-set-variable
 'package-pinned-packages
 '((auctex                . "gnu")
   (csharp-mode           . "melpa")
   (company               . "melpa")
   (company-auctex        . "melpa")
   (company-c-headers     . "melpa")
   (company-ghci          . "melpa")
   (company-jedi          . "melpa")
   (company-math          . "melpa")
   (company-web           . "melpa")
   (f                     . "melpa")
   (fill-column-indicator . "melpa")
   (fish-mode             . "melpa")
   (form-feed             . "melpa")
   (git-commit            . "melpa")
   (haskell-mode          . "melpa")
   (json-mode             . "melpa")
   (lua-mode              . "melpa")
   (markdown-mode         . "melpa")
   (nhexl-mode            . "gnu")
   (org                   . "melpa")
   (palette               . "melpa")
   (php-mode              . "melpa")
   (pretty-lambdada       . "melpa")
   (rainbow-delimiters    . "melpa")
   (scala-mode2           . "melpa")
   (tt-mode               . "melpa")
   (web-mode              . "melpa")
   (yaml-mode             . "melpa")
   (zenburn-theme         . "melpa")))

(defun bkalmar/packages/check-install ()
  "Install each package in `package-pinned-packages' not already installed."
  (interactive)
  (let (to-install)
    (when
        (dolist (pkg (mapcar 'car package-pinned-packages) to-install)
          (when (not (package-installed-p pkg))
            (push pkg to-install)))
      (package-refresh-contents)
      (mapc 'package-install to-install)
      (message "Installed %d new package%s" (length to-install)
               (if (= (length to-install) 1) "" "s")))))

(bkalmar/packages/check-install)

(defvar bkalmar/packages/checked-file
  (concat bkalmar/emacs-config-directory "elpa/last-checked")
  "Used by `bkalmar/packages/check-upgrade'.")

(defun bkalmar/packages/check-upgrade (age)
  "Call `package-list-packages' for upgrading, after user confirmation, if
`bkalmar/packages/checked-file' contains a time older than AGE."
  (interactive)
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

(bkalmar/packages/check-upgrade (days-to-time 7))

;;; manually installed packages

(defvar bkalmar/packages/manual
  (concat user-emacs-directory "packages/manual/")
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
