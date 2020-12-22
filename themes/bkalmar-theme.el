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

(require 'zenburn-theme)

(deftheme bkalmar
  "bkalmar is a theme which customizes little, except some packages' faces, most
notably `org`.")

(zenburn-with-color-variables
  (custom-theme-set-variables
   'bkalmar
   ;;; org
   '(org-todo-keyword-faces
     `(("CONS" . (:background "#a58259"))
       ("TODO" . (:background "#9c6363" :weight bold))
       ("STAR" . (:background "#9a9453" :weight bold))
       ("DONE" . (:background "#437f43"))
       ("CANC" . (:background "#328373"))))
   '(org-priority-faces
     `((?A . (:foreground "#f0dfaf" :weight bold))
       (?B . (:foreground "#dc8cc3" :weight bold))
       (?C . (:foreground "#8cd0d3")))))
  (custom-theme-set-faces
   'bkalmar
   ;;; markdown
   '(markdown-pre-face ((t . (:inherit (fixed-pitch font-lock-constant-face)))))
   '(markdown-inline-code-face ((t . (:inherit markdown-pre-face))))
   ;;; line-number-mode
   `(line-number
     ((t . (:background ,zenburn-bg+1 :foreground ,zenburn-fg :height 0.9
                        :width condensed :inherit fixed-pitch))))
   `(line-number-current-line
     ((t . (:foreground ,zenburn-yellow :weight bold :inherit line-number))))
   ;;; whitespace
   '(whitespace-space ((t . (:background nil :foreground "#6a6a6a"))))
   '(whitespace-hspace
     ((t . (:background nil :foreground nil :inherit whitespace-space))))
   '(whitespace-tab ((t . (:background nil :inherit whitespace-space))))
   '(whitespace-newline ((t . (:foreground nil :inherit whitespace-space))))
   '(whitespace-trailing
     ((t . (:background "#af7373" :inherit whitespace-space))))
   '(whitespace-line
     ((t . (:background "#5a445a" :foreground nil :slant italic))))
   '(whitespace-space-before-tab
     ((t . (:background nil :foreground nil :inherit whitespace-space))))
   '(whitespace-space-after-tab
     ((t . (:background nil :foreground nil
                        :inherit whitespace-space-before-tab))))
   '(whitespace-indentation
     ((t . (:background nil :foreground nil :inherit whitespace-space))))
   '(whitespace-empty ((t . (:background nil :inherit whitespace-trailing))))))

(provide-theme 'bkalmar)
