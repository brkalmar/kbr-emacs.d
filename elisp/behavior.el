;; before buffer is saved to file

(customize-set-variable 'require-final-newline t)

;; after buffer is saved to file

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

(add-hook 'after-save-hook 'bkalmar/auto-make-executable t)

;; integrate clipboard

(defun bkalmar/integrate-clipboard ()
  "Integrate the window system's clipboard and return t. Return nil if it cannot
be integrated."
  (if (and (equal window-system 'x) (display-selections-p))
      (progn (customize-set-variable 'x-select-enable-clipboard t)
             (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
             (setq interprogram-cut-function 'x-select-text)
             t)
    nil))

(bkalmar/integrate-clipboard)

;; editing

(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'read-quoted-char-radix 10)

;; coding
(prefer-coding-system 'utf-8)

;; locale
(setq system-time-locale "C")
