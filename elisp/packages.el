;; Add package archives.
;;
;; Check if neccessary packages are installed, install them if they aren't.
;;
;; Upgrade old packages.

(require 'package)

;; NOTE: Temporary fix for bug <https://debbugs.gnu.org/34341> encountered when
;; trying to upgrade elpa packages via TLS 1.3.  This should be removed once
;; updated to emacs 26.3, where the bug is fixed.
(customize-set-variable 'gnutls-algorithm-priority "normal:-vers-tls1.3")

;;; config

(customize-set-variable 'package-user-dir
                        (concat kbr/emacs-directory "packages/elpa/"))
(customize-set-variable 'package-gnupghome-dir
                        (concat package-user-dir "gnupg/"))
(customize-set-variable 'package-enable-at-startup nil)
(customize-set-variable 'package-load-list '(all))

;; Initialize already installed packages.
;;
;; NOTE: This must come before configurations of installed packages.
(package-initialize)

;;; ELPA packages

;; archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(customize-set-variable
 'package-pinned-packages
 '((auctex                . "gnu")
   (company               . "melpa")
   (company-auctex        . "melpa")
   (company-c-headers     . "melpa")
   (company-ghci          . "melpa")
   (company-irony         . "melpa")
   (company-math          . "melpa")
   (company-web           . "melpa")
   (csharp-mode           . "melpa")
   (diff-hl               . "gnu")
   (f                     . "melpa")
   (fill-column-indicator . "melpa")
   (fish-mode             . "melpa")
   (flycheck              . "melpa")
   (flycheck-irony        . "melpa")
   (flycheck-kotlin       . "melpa")
   (flycheck-rust         . "melpa")
   (form-feed             . "melpa")
   (forth-mode            . "melpa")
   (git-commit            . "melpa")
   (groovy-mode           . "melpa")
   (haskell-mode          . "melpa")
   (idris-mode            . "melpa")
   (irony                 . "melpa")
   (json-mode             . "melpa")
   (kotlin-mode           . "melpa")
   (linum-relative        . "melpa")
   (lua-mode              . "melpa")
   (markdown-mode         . "melpa")
   (multiple-cursors      . "melpa")
   (nhexl-mode            . "gnu")
   (org                   . "melpa")
   (palette               . "melpa")
   (php-mode              . "melpa")
   (pretty-lambdada       . "melpa")
   (racer                 . "melpa")
   (rainbow-delimiters    . "melpa")
   (rust-mode             . "melpa")
   (scala-mode            . "melpa")
   (smart-tabs-mode       . "melpa")
   (sparql-mode           . "melpa")
   (toml-mode             . "melpa")
   (tt-mode               . "melpa")
   (ttl-mode              . "melpa")
   (web-mode              . "melpa")
   (yaml-mode             . "melpa")
   (zenburn-theme         . "melpa")
   (zig-mode              . "melpa")))

(defun kbr/packages/check-install ()
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

(kbr/packages/check-install)

(defvar kbr/packages/checked-file
  (concat kbr/emacs-config-directory "elpa/last-checked")
  "Used by `kbr/packages/check-upgrade'.")

(defun kbr/packages/check-upgrade (age)
  "Call `package-list-packages' for upgrading, after user confirmation, if
`kbr/packages/checked-file' contains a time older than AGE."
  (interactive)
  (let ((filename (expand-file-name kbr/packages/checked-file)))
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

(kbr/packages/check-upgrade (days-to-time 7))

;;; manually installed packages

(defvar kbr/packages/manual
  (concat kbr/emacs-directory "packages/manual/")
  "Directory where manually installed packages are.")

;;; Package name
;; Version: YYYYMMDD
;; Updated: YYYY-MM-DD
;; Source: http://example.com/
;; Note: Something about build, etc...
;; (add-to-list 'load-path
;;              (concat (file-name-as-directory kbr/packages/manual)
;;                      "package-name-YYYYMMDD") t)

;;; Subtitles
;; Version: 1.100
;; Updated: 2015-05-04
;; Source: http://lists.gnu.org/archive/html/gnu-emacs-sources/2009-05/msg00007.html
;; Note: NONE
(add-to-list 'load-path (concat (file-name-as-directory kbr/packages/manual)
                                "subtitles-1.100") t)
