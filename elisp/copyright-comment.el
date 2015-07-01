;; Insertion of copyright comments.
;;
;; 2015  Bence Kalmar

(defun bkalmar/random-string (n &optional chars)
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

(defvar bkalmar/copyright-comment-license-alist
  '(("gpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")
    ("lgpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")
   ("agpl" . "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>."))
  "GNU license names and their associated copyright text used by
`bkalmar/insert-copyright-comment'.")

; " <-- this double quote fixes a font-lock bug with the above strings

(defvar bkalmar/copyright-comment-name "Bence Kalmar"
  "The name used by `bkalmar/insert-copyright-comment'.")

(defvar bkalmar/copyright-comment-mail "bkalmar1996@gmail.com"
  "The e-mail used by `bkalmar/insert-copyright-comment'.")

(defvar bkalmar/copyright-comment-fmt "\


Copyright %s  %s <%s>

%s"
  "Format string to be used by `bkalmar/insert-copyright-comment'.

It must have 4 unescaped \"%s\" format specifiers, as follows: year, name,
e-mail, copyright text.")

(defun bkalmar/insert-copyright-comment (license)
  "Insert a copyright notice comment at point.

LICENSE must be a key in `bkalmar/copyright-comment-license-alist', the name of the
license description to insert.  Interactively, the user is asked to choose.

The comment adheres to <http://www.gnu.org/licenses/gpl-howto.html>.  Its style
is extra-line.  After insertion, point is positioned at the end of the first
line.

See also `bkalmar/copyright-comment-name', `bkalmar/copyright-comment-mail',
`bkalmar/copyright-comment-fmt'."
  (interactive
   (list
    (completing-read "License type: " bkalmar/copyright-comment-license-alist
                     nil t)))
  (let ((comment-style 'extra-line)
        (beg (point))
        (license-text
         (cdr (assoc license bkalmar/copyright-comment-license-alist)))
        (empty-pattern (bkalmar/random-string 50 "abcdefghijklmnopqrstuvwxyz"))
        end)
    (when (null license-text)
      (error "%S is not a key in `bkalmar/copyright-comment-license-alist'"
             license))
    (insert
     (replace-regexp-in-string
      "^$" empty-pattern
      (format bkalmar/copyright-comment-fmt
              (format-time-string "%Y") bkalmar/copyright-comment-name
              bkalmar/copyright-comment-mail license-text)))
    (comment-region beg (point))
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward (regexp-quote empty-pattern) end t)
      (replace-match "" t))
    (goto-char beg)
    (move-end-of-line nil)))
