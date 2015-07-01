(require 'web-mode)

(defun bkalmar/choose-php-or-web-mode ()
  "Choose & call either `php-mode' or `web-mode' based on buffer contents."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      ;; pure-php scripts should not contain closing "?>"
      (if (search-forward "?>" nil t)
          (web-mode)
        (php-mode)))))

(add-to-list 'auto-mode-alist '("\\.php$" . bkalmar/choose-php-or-web-mode) t)
