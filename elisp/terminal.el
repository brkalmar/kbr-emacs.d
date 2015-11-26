;; Customized terminal emulation.
;;
;; 2015  Bence Kalmar

(require 'term)

(defvar bkalmar/terminal-interpreter "fish"
  "The interpreter `bkalmar/terminal' uses.")

(defvar bkalmar/terminal-name-cwd-fmt "%s"
  "Format string for buffer names of `bkalmar/terminal' using the CWD.

Must have one \"%s\" format specifier for the CWD.")

(defvar bkalmar/terminal-name-no-cwd "*terminal*"
  "Buffer name of `bkalmar/terminal' not using the CWD.")

(defun bkalmar/current-cwd ()
  "Get the current buffer's process' CWD, or nil if not accessible."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd-filename (format "/proc/%s/cwd" pid))
         (cwd (file-truename cwd-filename)))
    (if (equal cwd-filename cwd)
        ;; `file-truename' returns the string unchanged if not a symlink
        nil
      cwd)))

(defun bkalmar/terminal-rename-cwd ()
  "Rename the current terminal buffer to contain the CWD using
`bkalmar/terminal-name-cwd-fmt' if it is accessible, otherwise to
`bkalmar/terminal-name-no-cwd'."
  (let ((cwd (bkalmar/filename-last
              1 (abbreviate-file-name (bkalmar/current-cwd)) "⋮")))
    (rename-buffer
     (if (null cwd)
         bkalmar/terminal-name-no-cwd
       (format bkalmar/terminal-name-cwd-fmt cwd))
     t)))

(defadvice term-send-raw (after bkalmar/term-send-raw-rename)
  "Update buffer name after newline."
  (when (= last-command-event ?)
    ;; TODO: This function is called too quickly, before the CWD is changed in
    ;; the shell process itself.  Maybe call aynchronous (?) sleep for 0.1 s.
    (bkalmar/terminal-rename-cwd)))
(ad-activate 'term-send-raw)

(defadvice term-send-input (after bkalmar/term-send-input-rename)
  "Update buffer name after newline."
  (bkalmar/terminal-rename-cwd))
(ad-activate 'term-send-input)

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
  (bkalmar/terminal-rename-cwd)
  (bkalmar/terminal-set-keybindings))

(defun bkalmar/terminal-close ()
  "Close current terminal and kill its buffer."
  (interactive)
  (term-send-eof)
  ;; give time for the process to close
  (sleep-for 0.2)
  (kill-buffer))

