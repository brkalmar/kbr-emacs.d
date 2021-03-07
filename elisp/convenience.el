;;; convenience functions

(defun kbr/temp-buffer ()
  "Create & switch to new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "%temp%")))

(defun kbr/sort-lines-random (beg end)
  "Sort lines in region randomly, using `random' as a source of randomness."
  (interactive "r\n")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (a b) (eq (random 2) 1)))))))

(defun kbr/insert-header-guard ()
  "Insert a C/C++ ifndef/endif header guard at the beginning and end of the
  current buffer.  Use a preprocessor identifier based on the buffer's filename,
  making sure it is unique by appending random letters."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((preprocessor-id
             (concat
              (if (buffer-file-name)
                  ;; convert to all-uppercase
                  (upcase
                   ;; remove more than 1 consecutive underscores
                   (replace-regexp-in-string
                    "\\(_\\)\\(_+\\)" ""
                    ;; strip preceding & trailing underscores
                    (replace-regexp-in-string
                     "\\(^_+\\|_+$\\)" ""
                     ;; remove non-letter or -undescore characters
                     (replace-regexp-in-string
                      "[^a-zA-Z_]" ""
                      ;; replace dots and hyphens by underscores
                      (replace-regexp-in-string
                       "[.-]+" "_"
                       ;; take the filename
                       (file-name-nondirectory (buffer-file-name))
                       t t) t t) t t) t t 1))
                (error "this buffer has no filename"))
              "_" (kbr/random-string 10 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
        (goto-char (point-min))
        (insert (format "#ifndef %s\n#define %s\n\n"
                        preprocessor-id preprocessor-id))
        (goto-char (point-max))
        (when (not (= (char-before) ?\n))
          (insert ?\n))
        (insert (format "\n#endif /* %s */\n" preprocessor-id))))))

(defun kbr/random-string (n &optional chars)
  "Get a string of N random characters from `random'.

If CHARS is non-nil, it must be a string containing characters to choose from.
Otherwise choose from all characters in the allowable range."
  (random t)
  (let ((res (make-string n 0)))
    (dotimes (i n res)
      (aset
       res i
       (if (null chars)
           (random (1+ (max-char)))
         (aref chars (random (length chars))))))))
