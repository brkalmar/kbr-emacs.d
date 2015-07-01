(require 'x-win)

(defun emacs-session-filename (session-id)
  (let ((dir (concat bkalmar/emacs-config-directory "x-win/")))
    (mkdir dir t)   
    (expand-file-name (concat dir "session." session-id))))
