;; Customized terminal emulation.
;;
;; 2015  Bence Kalmar

(defvar bkalmar/terminal-interpreter "fish"
  "The interpreter `bkalmar/terminal' uses.")

(defvar bkalmar/terminal-name-fmt "*terminal∙%02d*"
  "Format string for buffer names of `bkalmar/terminal'.

Must have one \"%d\" format specifier for the number of the terminal.")

(defun bkalmar/terminal-rename-uniquely ()
  "Rename the current buffer to a unique name using
`bkalmar/terminal-name-fmt'."
  (rename-buffer
   (do* ((n 1 (1+ n))
         (name (format bkalmar/terminal-name-fmt n)
               (format bkalmar/terminal-name-fmt n)))
       ((null (get-buffer name)) name))))

(defun bkalmar/terminal-set-keybindings ()
  "Set local keybindings needed in a terminal."
  ;; use C-x since C-c keybindings are inaccessible inside a terminal
  (local-set-key (kbd "C-x e") 'bkalmar/terminal)
  (local-set-key (kbd "C-x w") 'bkalmar/terminal-close))

(defun bkalmar/terminal ()
  "Start a new terminal with a unique name using `ansi-term' with the
interpreter `bkalmar/terminal-interpreter'."
  (interactive)
  (ansi-term bkalmar/terminal-interpreter)
  (bkalmar/terminal-rename-uniquely)
  (bkalmar/terminal-set-keybindings))

(defun bkalmar/terminal-close ()
  "Close current terminal and kill its buffer."
  (interactive)
  (term-send-eof)
  ;; give time for the process to close
  (sleep-for 0.2)
  (kill-buffer))

