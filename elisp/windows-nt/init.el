;; Custom startup file for Windows NT.
;;
;; 2014  Bence Kalmar

(setq bkalmar/userdir
      (mapconcat 'identity
                 (subseq (split-string (expand-file-name "~") "/") 0 3) "/"))

(load-file (concat bkalmar/emacs-elisp-directory "windows-nt/themes.el"))
