;; Add neccessary package archives.
;; Check if neccessary packages are installed, install them if they aren't.
;; Upgrade old packages.
;; 
;; 2014-01-11 / 2014-01-12
;; AlbusDrachir

;;; Neccessary to actually initialize the package system.
(require 'package)

;;;; Archives

;;; Marmalade
;; Contains: json-mode
;;           fill-column-indicator
;;           jam-mode
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;;; Initialize all ELPA packages.

(setq package-user-dir "~/.emacs.d/packages/elpa")
(setq package-enable-at-startup nil)
(setq package-load-list '(all))
(package-initialize)

(defvar init-packages-check-list
  '(json-mode
    fill-column-indicator
    jam-mode)
  "Packages used by `init-packages-check'.

Must be a list, where each element is a package name (a symbol).")

(defun init-packages-check ()
  "Install or upgrade each package in `init-packages-check-list'."
  (package-refresh-contents)
  (let (to-install)
    (dolist (pkg init-packages-check-list)
      (when (not (package-installed-p pkg))
        (add-to-list 'to-install pkg t)))
    (dolist (pkg (package-menu--find-upgrades))
      (add-to-list 'to-install pkg t))
    (when to-install
      (message "The following packages will be installed: %s"
	       (mapconcat 'symbol-name to-install ", "))
      (mapc 'package-install to-install))))

(init-packages-check)

;;;; Initialize manually installed packages.

(defvar init-packages-manual
  "~/.emacs.d/packages/manual"
  "Directory where manually installed packages are.")

;;; Lua mode
;; Version: 20130419
;; Updated: 2014-01-11
;; Source: https://github.com/immerrr/lua-mode
(add-to-list 'load-path (concat (file-name-as-directory init-packages-manual)
                                "lua-mode-20130419") t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode) t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode) t)
