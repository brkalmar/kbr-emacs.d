(require 'calendar)

(setq calendar-date-style 'iso)

;; start week with Monday
(setq calendar-week-start-day 1)

;; ISO week numbers

(defface bkalmar/calendar-iso-week
  '((t . (:height 0.9 :inherit font-lock-builtin-face)))
  "Face used to highlight ISO week numbers in calendar.")

(setq calendar-intermonth-text
      '(propertize
        (format "%2d" (car (calendar-iso-from-absolute
                           (calendar-absolute-from-gregorian
                            (list month day year)))))
        'font-lock-face 'bkalmar/calendar-iso-week))
