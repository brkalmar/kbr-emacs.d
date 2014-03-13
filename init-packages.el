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
      '(("melpa" . package-filter)
        ("melpa" . json-mode)
        ("melpa" . fill-column-indicator)
        ("melpa" . git-commit-mode)
        ("melpa" . markdown-mode)
        ("melpa" . web-mode)
        ("melpa" . lua-mode)
        ("marmalade" . nhexl-mode)
        ("marmalade" . jam-mode)))

(defun init-packages-refresh-archives (age)
  "Refresh archives & mark in file as refreshed if they haven't been refreshed
in more than AGE time.

Return t if the archives have been refreshed, nil otherwise."
  (let ((filename (expand-file-name "~/.emacs.d/packages/elpa/.last-refresh"))
        (old-before (time-subtract (current-time) age))
        (exists t)
        (modified nil)
        contents)
    (with-temp-buffer
      (if (file-exists-p filename)
          (progn
            (insert-file-contents-literally filename)
            (setq contents
                  (buffer-substring-no-properties (point-min) (point-max))))
        (setq exists nil))
      (when (or (not exists) (time-less-p (date-to-time contents) old-before))
        (setq modified t)
        (package-refresh-contents)
        (setq contents (format-time-string "%Y-%m-%dT%H:%M:%S")))
      (delete-region (point-min) (point-max))
      (insert contents)
      (write-file filename))
    modified))

(defun init-packages-check ()
  "Install or upgrade each cdr in `package-archive-enable-alist'."
  (and
   (init-packages-refresh-archives (days-to-time 7))
   (let (to-install)
     (dolist (pkg package-archive-enable-alist)
       (setq pkg (cdr pkg))
       (when (not (package-installed-p pkg))
         (add-to-list 'to-install pkg t)))
     (dolist (pkg (package-menu--find-upgrades))
       (add-to-list 'to-install pkg t))
     (when to-install
       (message "The following packages will be installed: %s"
                (mapconcat 'symbol-name to-install ", "))
       (mapc 'package-install to-install)))))

(init-packages-check)

;;;; Initialize manually installed packages.

(defvar init-packages-manual
  "~/.emacs.d/packages/manual"
  "Directory where manually installed packages are.")

;;; Lua mode
;; Version: 20130419
;; Updated: 2014-01-11
;; Source: https://github.com/immerrr/lua-mode
;; (add-to-list 'load-path (concat (file-name-as-directory init-packages-manual)
;;                                 "lua-mode-20130419") t)
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode) t)
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode) t)
