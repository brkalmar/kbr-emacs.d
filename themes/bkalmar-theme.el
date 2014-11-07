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
  "bkalmar is a theme which customizes little, except some package's faces, most
notably `org`.")

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

(provide-theme 'bkalmar)
