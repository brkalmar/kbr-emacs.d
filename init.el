;; Custom startup file for GNU-emacs.
;;
;; 2013-05-03 / 2014-01-13
;; AlbusDrachir

(cond
 ((equal system-type 'gnu/linux)
  (message "Loading GNU/Linux specific init file...")
  (load-file "~/.emacs.d/init-gnu-linux.el"))
 ((equal system-type 'windows-nt)
  (message "Loading Windows NT specific init file...")
  (load-file "~/.emacs.d/init-windows-nt.el"))
 (t
  (message "Could not find appropriate config file for system type: %s"
           system-type)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e4cac8bc0265e17d08a8ad4629a6c95ab751dfceec3c55110bc76aaf954962ed" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
