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

It is meant for graphic displays.  It customizes some package's faces, notably
`org`.")

(custom-theme-set-variables
 'bkalmar
 ;;; org
 '(org-todo-keyword-faces
   '(("CONS" . (:background "#d34c09" :weight bold :slant italic))
     ("TODO" . (:background "#C01414" :weight bold :slant italic))
     ("STAR" . (:background "#1643B2" :weight bold :slant italic))
     ("DONE" . (:background "#0e9d07"))
     ("CANC" . (:background "#789d07"))))
 '(org-priority-faces
   '((?A . (:foreground "#fffc00" :weight bold))
     (?B . (:foreground "#0cff00" :weight bold))
     (?C . (:foreground "#00d8ff")))))

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
 '(escape-glyph ((t . (:foreground "#ccd7e5" :slant italic))))
 '(fringe ((t . (:background "#0a0a0a"))))
 '(header-line ((t . (:inherit mode-line :box nil))))
 '(highlight
    ((t . (:foreground "#ffff00" :background "#0000ff"))))
 '(isearch ((t . (:foreground "#000000" :background "#db7093"))))
 ;; isearch-fail
 '(lazy-highlight ((t . (:foreground "#000000" :background "#96cdcd"))))
 '(minibuffer-prompt ((t . (:foreground "#963bdc" :weight bold))))
 '(mode-line
   ((t . (:foreground "#eeeeee" :background "#1a1a1a"
                      :box (:line-width -1 :color "#bbbbbb")))))
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
 ;;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t . (:inherit default))))
 '(rainbow-delimiters-depth-2-face ((t . (:foreground "#ff2222"))))
 '(rainbow-delimiters-depth-3-face ((t . (:foreground "#22cc44"))))
 '(rainbow-delimiters-depth-4-face ((t . (:foreground "#cc33cc"))))
 '(rainbow-delimiters-depth-5-face ((t . (:foreground "#ffdd22"))))
 '(rainbow-delimiters-depth-6-face ((t . (:foreground "#5555ff"))))
 '(rainbow-delimiters-depth-7-face ((t . (:foreground "#ff6022"))))
 '(rainbow-delimiters-depth-8-face ((t . (:foreground "#33ffff"))))
 '(rainbow-delimiters-depth-9-face ((t . (:foreground "#aaee22"))))
 '(rainbow-delimiters-unmatched-face ((t . (:background "#660000")))))

(provide-theme 'bkalmar)
