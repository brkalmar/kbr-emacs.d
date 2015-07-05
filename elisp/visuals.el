;; Visual improvements
;;
;; 2015  Bence Kalmar

(defun bkalmar/init-visuals ()
  "Toggle fullscreen; disappear scrollbar."
  (interactive)
  (toggle-frame-maximized)
  (when (display-graphic-p)
    (scroll-bar-mode -1))
  ;; temporary fix for Debian
  (set-cursor-color "#ffcc00"))

(bkalmar/init-visuals)

(defun bkalmar/after-make-frame (new-frame)
  "Call `bkalmar/init-visuals' on `new-frame'."
  (select-frame new-frame)
  (bkalmar/init-visuals))

(add-hook 'after-make-frame-functions 'bkalmar/after-make-frame t)

(customize-set-variable 'inhibit-startup-screen t)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(size-indication-mode 1)
(menu-bar-mode -1)

;; frame resize
(customize-set-variable 'frame-resize-pixelwise t)

;; half-width fringes
(fringe-mode 4)

;; selections
(transient-mark-mode 1)
(delete-selection-mode 1)

;; numbers of columns to fill
(customize-set-variable 'fill-column 80)

;; line and column numbers
(line-number-mode -1)
(column-number-mode 1)

;; frame & icon titles

(defun bkalmar/buffer-file-truename-last (n &optional prefix not-abs)
  "Return string of last N path elements of `buffer-file-truename' or nil if
`buffer-file-truename' is nil.

If PREFIX is non-nil, prefix the returned string with it.

If NOT-ABS is non-nil, do not prefix the string if it's an absolute path."
  (when buffer-file-truename
    (let ((elems (split-string buffer-file-truename "/"))
           elems-last
           res
           pre
           post
           is-full)
      ;; starts with a "/"
      (when (eq (nth 0 elems) "")
        (setq pre "/")
        (pop elems))
      ;; ends with a "/"
      (when (eq (car (last elems)) "")
        (setq post "/")
        (nbutlast elems))
      (setq elems-last (last elems n))
      (setq is-full (= (length elems) (length elems-last)))
      (setq res (concat (if is-full pre) (mapconcat 'identity elems-last "/") post))
      (if (and not-abs is-full)
          res
        (concat prefix res)))))

(setq frame-title-format
      '((:eval (or (bkalmar/buffer-file-truename-last 2 "•••/" t) "%b"))
        " (%I)"))

(setq icon-title-format "%b")
