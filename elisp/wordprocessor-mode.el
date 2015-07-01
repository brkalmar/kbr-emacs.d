;; Minor mode `wordprocessor-mode'.
;;
;; 2015  Bence Kalmar

(defvar bkalmar/disable-themes-list ()
  "The themes that have been last disabled by `bkalmar/disable-themes', if any.")

(defun bkalmar/disable-themes ()
  "Disable all themes."
  (interactive)
  (message "Disabling all themes...")
  (setq bkalmar/disable-themes-list (mapc 'disable-theme custom-enabled-themes)))

(defun bkalmar/enable-themes ()
  "Enable all themes that have been disabled.
If no themes have been disabled, do nothing."
  (interactive)
  (if bkalmar/disable-themes-list
      (progn
        (message "Enabling disabled themes...")
        (mapc 'enable-theme bkalmar/disable-themes-list))
    (message "No disabled themes to enable.")))

(defvar bkalmar/wordprocessor-fringe-bg nil
  "Original background for face `fringe', modified by `bkalmar/wordprocessor-mode',
if any.")

(define-minor-mode bkalmar/wordprocessor-mode
  "Minor mode for a \"word processor look\" in the current window.  Uses the
default theme and some margins on both sides."
  nil " WP" nil
  (if bkalmar/wordprocessor-mode
      (progn
        ;; disable linum mode
        (linum-mode -1)
        ;; wordwrap
        (visual-line-mode +1)
        ;; adjust font size
        (text-scale-adjust +1.5)
        ;; disable themes
        (bkalmar/disable-themes)
        ;; margins
        (setq left-margin-width 20)
        (setq right-margin-width 20)
        ;; invisible fringes
        (setq bkalmar/wordprocessor-fringe-bg (face-attribute 'fringe :background))
        (set-face-attribute 'fringe nil :background
                            (face-attribute 'default :background))
        ;; update window
        (set-window-buffer nil (current-buffer)))
    ;; enable linum mode
    (linum-mode +1)
    ;; no wordwrap
    (visual-line-mode -1)
    ;; reset font size
    (text-scale-adjust -1.5)
    ;; visible fringes
    (set-face-attribute 'fringe nil :background bkalmar/wordprocessor-fringe-bg)
    ;; enable themes
    (bkalmar/enable-themes)
    ;; margins
    (setq left-margin-width 0)
    (setq right-margin-width 0)
    ;; update window
    (set-window-buffer nil (current-buffer))))

