;; Get & display information about fonts.
;;
;; 2015  Bence Kalmar

;; source: https://gist.github.com/haxney/3055728
(defun bkalmar/font-is-mono-p (font-family)
  "Return `t' if `font-family' appears to be a monospace font's family, `nil'
otherwise."
  (let (width)
    (with-temp-buffer
      (set-window-buffer nil (current-buffer))
      (text-scale-set 4)
      (insert (propertize "a b c d l" 'face `(:family ,font-family)))
      (goto-char (line-end-position))
      (setq width (car (posn-x-y (posn-at-point))))
      (newline)
      (insert (propertize "v w x y m" 'face `(:family ,font-family)))
      (goto-char (line-end-position))
      (eq width (car (posn-x-y (posn-at-point)))))))

(defvar bkalmar/compare-fonts-history '()
  "History list for `bkalmar/compare-fonts'.")

(defvar bkalmar/compare-fonts-invalid-char ?�
  "Used to replace those characters in the test string — after insertion — which
have no font / are displayed in an overloaded font.")

(defvar bkalmar/compare-fonts-test-text
  ;; source: http://www.columbia.edu/~fdc/utf8/index.html#glass
  "The quick brown fox jumps over the lazy dog. 1 l I i 0 O o.  Tudok üveget enni anélkül hogy bajom lenne tőle.  Aš galiu valgyti stiklą ir jis manęs nežeidžia.  Μπορώ να φάω σπασμένα γυαλιά χωρίς να πάθω τίποτα.  Можам да јадам стакло, а не ме штета.  我能吞下玻璃而不傷身體。  나는 유리를 먹을 수 있어요. 그래도 아프지 않아요.  ∀∁∅∋∉ℕ∑∜∞∧∩∪∟∠∭∰∴≅≈≣≤≫≹⊃⊭⋅⋘ ℃Ω ⏚⏦  .من می توانم بدونِ احساس درد شيشه بخورم  ﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥  .איך קען עסן גלאָז און עס טוט מיר נישט װײ  ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ"
  "Default test text for `bkalmar/compare-fonts'.  Normally should not have a
  closing newline.")

;; source: https://gist.github.com/haxney/3055728
(defun bkalmar/compare-fonts (&optional test-text font-type only-regular-text)
  "Display a list of font faces with some test text for each.

If `test-text' is `nil', use `bkalmar/compare-fonts-test-text'.  Interactively,
with a single C-u prefix argument ask for a custom test string, use
`bkalmar/compare-fonts-test-text' otherwise.

If `font-type' is `nil', display all fonts; otherwise it must be one of
`:monospace' or `:proportional' to display only those type of fonts.
Interactively, ask what type of fonts to display.

If `only-regular-text' is non-`nil', display only the regularly formatted test
text, not the bold or italic ones for each font.  Interactively, ask whether to
display only regular text."
  (interactive
   (let (test-text font-type only-regular-text)
     (when (equal current-prefix-arg '(4))
       (setq test-text
             (read-string "Test text for each font: " nil
                          'bkalmar/compare-fonts-history
                          bkalmar/compare-fonts-test-text)))
     (setq font-type
           (cdr (assoc
                 (completing-read "Type of fonts to display (default all): "
                                  '("monospace" "proportional" "all") nil t nil
                                  'bkalmar/compare-fonts-history "all")
                 '(("all" . :all)
                   ("monospace" . :monospace)
                   ("proportional" . :proportional)))))
     (setq only-regular-text
           (not (y-or-n-p "Show bold and italic test text? ")))
     (list test-text font-type only-regular-text)))
  (when (null test-text)
    (setq test-text bkalmar/compare-fonts-test-text))
  (when (null font-type)
    (setq font-type :all))
  (let ((count 0)
        ;; predicate for whether to display a font family; based on `font-type'
        (display-font-p
         (cond ((eq font-type :all) (lambda (x) t))
               ((eq font-type :monospace) 'bkalmar/font-is-mono-p)
               ((eq font-type :proportional)
                (lambda (x) (not (bkalmar/font-is-mono-p x))))
               (t (error "Unrecognized `font-type' argument"))))
        pos-text-beg
        pos-text-end)
    (pop-to-buffer "*Fonts*")
    (erase-buffer)
    (dolist (font-family (sort (delete-dups (font-family-list)) 'string<))
      (when (funcall display-font-p font-family)
        (setq count (1+ count))
        ;; insert font's name
        (insert (format "%s:\n" (propertize font-family 'face '(:weight bold))))
        (setq pos-text-beg (point))
        ;; insert test text
        (insert
         (propertize (format "%s\n" test-text) 'face `(:family ,font-family)))
        (when (not only-regular-text)
          (insert
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :slant italic))
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :weight bold))
           (propertize (format "%s\n" test-text)
                       'face `(:family ,font-family :slant italic
                                       :weight bold))))
        (newline)
        ;; replace fontless/overloaded characters in inserted test text
        (setq pos-text-end (point))
        (goto-char pos-text-beg)
        (while (not (= (point) pos-text-end))
          (if (and
               ;; ignore newlines
               (not (= (char-after) ?\n))
               (or
                ;; character cannot be displayed
                (null (font-at (point)))
                ;; character is displayed in font other than desired (overloaded
                ;; font)
                (not (eq (font-get (font-at (point)) :family)
                         (intern font-family)))))
              (progn
                (delete-char 1)
                (insert bkalmar/compare-fonts-invalid-char))
            (forward-char)))))
    ;; insert number of fonts displayed
    (goto-char (point-min))
    (insert (format "%d fonts\n\n" count))))
