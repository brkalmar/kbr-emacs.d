;; `bkalmar` emacs custom theme, based on `dark-emacs` by Suvayu Ali.
;; 
;; Authors: Suvayu Ali <https://github.com/suvayu>
;;          Bence Kalmar <bkalmar1996@gmail.com>
;; Copyright (C) 2012  Suvayu Ali
;;               2014  Bence Kalmar
;; GNU GPLv3 - http://www.gnu.org/licenses/gpl-3.0.txt

(deftheme bkalmar
  "bkalmar is a dark-background theme with saturated foreground colors.

It is meant for graphic displays.")

(custom-theme-set-faces
 'bkalmar
 ;;; standard
 '(bold ((t . (:inherit default :weight bold))))
 '(bold-italic ((t . (:inherit (bold italic)))))
 '(default ((t . (:foreground "#eeeeee" :background "#000000"))))
 '(fixed-pitch
   ((t . (:inherit default :family :foreground "#f5f5dc" :slant reverse-italic))))
 '(italic ((t . (:inherit default :foreground "#ffebcd" :slant italic))))
 '(shadow ((t . (:inherit default :foreground "#8c8c8c"))))
 '(underline ((t . (:inherit default :underline t :foreground "#00ff00"))))
 ;; '(variable-pitch)
 ;;; standard highlight
 '(cursor ((t . (:background "#ff0000"))))
 '(escape-glyph)
 '(fringe)
 '(header-line ((t . (:inherit mode-line))))
 '(highlight
   ((t . (:inherit default :foreground "#ffff00" :background "#0000ff"))))
 '(isearch)
 '(lazy-highlight)
 '(minibuffer-prompt ((t . (:foreground "dark cyan" :weight bold))))
 '(mode-line)
 '(mode-line-buffer-id)
 '(mode-line-highlight)
 '(mode-line-inactive)
 '(mouse)
 '(nobreak-space ((t . (:inherit escape-glyph))))
 '(query-replace)
 '(region ((t . (:inherit default :inverse-video t))))
 '(secondary-selection ((t . (:inherit region))))
 '(tooltip)
 '(trailing-whitespace ((t . (:inherit default :background "#cd0000"))))
 '(vertical-border)
 ;;; standard misc
 '(link ((t . (:inherit default :foreground "#00ffff" :underline t
                        :weight extra-bold))))
 '(link-visited ((t . (:inherit link :foreground "#ff00ff"))))
 ;; '(match)
 ;;; completions buffer
 ;; '(completions-annotations)
 ;; '(completions-common-part ((t . (:foreground "forest green"))))
 ;; '(completions-first-difference ((t . (:weight bold :foreground "salmon"))))
 ;;; show-paren-mode
 ;; '(show-paren-match ((t . (:background "SlateBlue1"))))
 ;; '(show-paren-mismatch ((t . (:foreground "white" :background "magenta"))))
 ;;; diff-mode
 ;; '(diff-added ((t . (:inherit default "#335533"))))
 ;; '(diff-file-header ((t . (:inherit diff-header :weight bold))))
 ;; '(diff-header ((t . (:inherit default :background "grey45"))))
 ;; '(diff-indicator-added ((t . (:inherit diff-added :weight bold))))
 ;; '(diff-indicator-removed ((t . (:inherit diff-removed :weight bold))))
 ;; '(diff-refine-added ((t . (:inherit default :background "#22aa22"))))
 ;; '(diff-refine-removed ((t . (:inherit default :background "#aa2222"))))
 ;; '(diff-removed ((t . (:inherit default :background "#553333"))))
 ;;; font-lock-mode
 ;; '(font-lock-builtin-face ((t . (:inherit default :foreground "gold"))))
 ;; '(font-lock-comment-delimiter-face ((t . (:inherit font-lock-comment-face))))
 ;; '(font-lock-comment-face ((t . (:inherit italic :foreground "cyan3"))))
 ;; '(font-lock-constant-face ((t . (:inherit default :foreground "LightGoldenrod2"))))
 ;; '(font-lock-doc-face)
 ;; '(font-lock-function-name-face
 ;;   ((t . (:inherit default :weight bold :foreground "white"))))
 ;; '(font-lock-keyword-face ((t . (:inherit italic :foreground "firebrick1"))))
 ;; '(font-lock-negation-char-face)
 ;; '(font-lock-preprocessor-face
 ;;   ((t . (:inherit font-lock-builtin-face :foreground "IndianRed3"))))
 ;; '(font-lock-regexp-grouping-backslash)
 ;; '(font-lock-regexp-grouping-construct)
 ;; '(font-lock-string-face ((t . (:inherit default :foreground "lawn green" ))))
 ;; '(font-lock-type-face ((t . (:inherit italic :foreground "orchid"))))
 ;; '(font-lock-variable-name-face ((t . (:inherit bold :foreground "orange"))))
 ;; '(font-lock-warning-face ((t . (:inherit bold :foreground "Red1"))))
)

(provide-theme 'bkalmar)
