;; `bkalmar` emacs custom theme, based on `dark-emacs` by Suvayu Ali.
;; 
;; Copyright 2012  Suvayu Ali <https://github.com/suvayu>
;; Copyright 2014  Bence Kalmar <bkalmar1996@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(deftheme bkalmar
  "bkalmar is a dark-background theme with saturated foreground colors.

It is meant for graphic displays.")

(custom-theme-set-faces
 'bkalmar
 ;;; standard
 ;; bold
 ;; bold-italic
 '(default ((t . (:foreground "#eeeeee" :background "#000000"))))
 '(fixed-pitch ((t . (:family "Monospace" :foreground "#ffbebe"))))
 ;; italic
 '(shadow ((t . (:foreground "#7c7c7c"))))
 ;; underline
 '(variable-pitch ((t . (:family "Sans Serif" :foreground "#ffbebe"))))
 ;;; standard highlight
 '(cursor ((t . (:background "#ffcc00"))))
 '(escape-glyph ((t . (:foreground "#ccd7e5" :slant 'italic))))
 '(fringe ((t . (:background "#0a0a0a"))))
 '(header-line ((t . (:inherit mode-line :box nil))))
 '(highlight
    ((t . (:foreground "#ffff00" :background "#0000ff"))))
 '(isearch ((t . (:foreground "#000000" :background "#db7093"))))
 ;; isearch-fail
 '(lazy-highlight ((t . (:foreground "#000000" :background "#96cdcd"))))
 '(minibuffer-prompt ((t . (:foreground "#963bdc" :weight bold))))
 '(mode-line
   ((t . (:background "#1a1a1a" :box (:line-width -1 :color "#bbbbbb")))))
 '(mode-line-buffer-id ((t . (:foreground "#fa3535" :weight bold))))
 '(mode-line-emphasis ((t . (:slant italic))))
 '(mode-line-highlight ((t . (:weight bold))))
 '(mode-line-inactive ((t . (:inherit mode-line :background "#0a0a0a"
                                      :box (:line-width -1 :color "#1a1a1a")))))
 ;; mouse
 ;; nobreak-space
 ;; query-replace
 '(region ((t . (:background "#4e3c19"))))
 '(secondary-selection ((t . (:inherit region :background "#19454e"))))
 ;; tooltip
 '(trailing-whitespace ((t . (:background "#ff0000"))))
 '(vertical-border ((t . (:foreground "#bbbbbb"))))
 ;;; standard misc
 ;; link
 ;; link-visited
 '(match ((t . (:background "#268829"))))
 ;;; completions buffer
 ;; '(completions-annotations)
 ;; '(completions-common-part ((t . (:foreground "forest green"))))
 ;; '(completions-first-difference ((t . (:weight bold :foreground "salmon"))))
 ;;; show-paren-mode
 ;; '(show-paren-match ((t . (:background "SlateBlue1"))))
 ;; '(show-paren-mismatch ((t . (:foreground "white" :background "magenta"))))
 ;;; diff-mode
 ;; '(diff-added ((t . ("#335533"))))
 ;; '(diff-file-header ((t . (:inherit diff-header :weight bold))))
 ;; '(diff-header ((t . (:background "grey45"))))
 ;; '(diff-indicator-added ((t . (:inherit diff-added :weight bold))))
 ;; '(diff-indicator-removed ((t . (:inherit diff-removed :weight bold))))
 ;; '(diff-refine-added ((t . (:background "#22aa22"))))
 ;; '(diff-refine-removed ((t . (:background "#aa2222"))))
 ;; '(diff-removed ((t . (:background "#553333"))))
 ;;; font-lock-mode
 ;; '(font-lock-builtin-face ((t . (:foreground "gold"))))
 ;; '(font-lock-comment-delimiter-face ((t . (:inherit font-lock-comment-face))))
 ;; '(font-lock-comment-face ((t . (:inherit italic :foreground "cyan3"))))
 ;; '(font-lock-constant-face ((t . (:foreground "LightGoldenrod2"))))
 ;; '(font-lock-doc-face)
 ;; '(font-lock-function-name-face
 ;;   ((t . (:weight bold :foreground "white"))))
 ;; '(font-lock-keyword-face ((t . (:inherit italic :foreground "firebrick1"))))
 ;; '(font-lock-negation-char-face)
 ;; '(font-lock-preprocessor-face
 ;;   ((t . (:inherit font-lock-builtin-face :foreground "IndianRed3"))))
 ;; '(font-lock-regexp-grouping-backslash)
 ;; '(font-lock-regexp-grouping-construct)
 ;; '(font-lock-string-face ((t . (:foreground "lawn green" ))))
 ;; '(font-lock-type-face ((t . (:inherit italic :foreground "orchid"))))
 ;; '(font-lock-variable-name-face ((t . (:inherit bold :foreground "orange"))))
 ;; '(font-lock-warning-face ((t . (:inherit bold :foreground "Red1"))))
 ;;; linum
 ;; linum
)

(provide-theme 'bkalmar)
