;; Custom startup file for Windows NT.
;;
;; 2014  Bence Kalmar

(require 'cl)

;;;; Functions

(setq init-userdir
      (mapconcat 'identity
                 (subseq (split-string (expand-file-name "~") "/") 0 3) "/"))

;; set diff & ediff commands to specified file if it exists
(let ((diff-exec-file "C:\\Users\\be02029606\\_\\git-1.8.4\\bin\\diff.exe"))
  (when (file-readable-p diff-exec-file)
    (setq diff-command diff-exec-file)
    (setq ediff-diff-program diff-exec-file)))

;;;; Themes

(setq init-preferred-fonts-monospace
      '("DejaVu Sans Mono" "Unifont" "Consolas" "Bitstream Vera Sans Mono"
        "Courier New" "Lucida Sans Typewriter" "Letter Gothic Std"))

(setq init-preferred-fonts-proportional
      '("DejaVu Sans" "Arial" "Candara" "Calibri" "Segoe UI" "Trebuchet MS"
        "Corbel"))
