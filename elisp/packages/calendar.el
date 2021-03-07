(require 'calendar)

(customize-set-variable 'calendar-date-style 'iso)

;; start week on Monday
(customize-set-variable 'calendar-week-start-day 1)

;;; ISO week numbers

(defface kbr/calendar-iso-week
  '((t . (:height 0.9 :inherit font-lock-builtin-face)))
  "Face used to highlight ISO week numbers in calendar.")

(customize-set-variable
 'calendar-intermonth-text
 '(propertize
   (format "%2d" (car (calendar-iso-from-absolute
                       (calendar-absolute-from-gregorian
                        (list month day year)))))
   'font-lock-face 'kbr/calendar-iso-week))
