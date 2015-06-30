;; Custom startup file for GNU/Linux.
;; 
;; 2014  Bence Kalmar

;;;; Functions & variables

(setq bkalmar/userdir (expand-file-name "~"))

;; This is unnecessary on windows, as files don't have an executable property.
(defun bkalmar/auto-make-executable ()
  "Make current buffer's file executable if begins whith a shebang."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (concat "chmod u+x "
                              (shell-quote-argument buffer-file-name)))
       (message "Made executable %s" buffer-file-name)))

;; Not needed on windows.
(defun bkalmar/integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (setq x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil))

;;;; Hooks and similar

;; after buffer is saved to file
(add-hook 'after-save-hook 'bkalmar/auto-make-executable t)

;;; visual
(setq visible-bell t)

;; integrate clipboard
(bkalmar/integrate-clipboard)

;;;; Themes

(setq bkalmar/preferred-fonts-monospace
      '("DejaVu Sans Mono" "Unifont" "VL Gothic" "Liberation Mono" "FreeMono"
        "Andale Mono" "Droid Sans Mono"))

(setq bkalmar/preferred-fonts-proportional
      '("DejaVu Sans" "Arial" "VL PGothic" "Linux Biolinum O" "Droid Sans"
        "FreeSans" "Trebuchet MS" "Lato" "Liberation Sans" "Carlito"))
