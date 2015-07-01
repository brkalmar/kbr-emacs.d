;; Custom startup file for GNU/Linux.
;; 
;; 2014  Bence Kalmar

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

;; after buffer is saved to file
(add-hook 'after-save-hook 'bkalmar/auto-make-executable t)

;; integrate the clipboard
;; not needed on windows.
(defun bkalmar/integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (setq x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil))

;; integrate clipboard
(bkalmar/integrate-clipboard)

;;; visual
(setq visible-bell t)

(load-file (concat bkalmar/emacs-elisp-directory "gnu-linux/themes.el"))
