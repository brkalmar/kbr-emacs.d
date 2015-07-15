;; Customized terminal emulation.
;;
;; 2015  Bence Kalmar

(require 'term)

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

;; source: http://www.cs.columbia.edu/~ricardo/misc/Eterm_reference.html
(defconst bkalmar/terminal-f-key-alist
  '((f1  . "\e[11~")
    (f2  . "\e[12~")
    (f3  . "\e[13~")
    (f4  . "\e[14~")
    (f5  . "\e[15~")
    (f6  . "\e[17~")
    (f7  . "\e[18~")
    (f8  . "\e[19~")
    (f9  . "\e[20~")
    (f10 . "\e[21~")
    (f11 . "\e[23~")
    (f12 . "\e[24~"))
  "Eterm escape sequences for the keys F1–F12.")

(defun bkalmar/terminal-send-f-key ()
  "Send the escape sequence belonging to the pressed F-key to `term`."
  (interactive)
  (term-send-raw-string
   (cdr (assoc last-input-event bkalmar/terminal-f-key-alist))))

(dolist (e bkalmar/terminal-f-key-alist)
  (define-key term-raw-map (read-kbd-macro (format "<%s>" (car e)))
    'bkalmar/terminal-send-f-key))

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

