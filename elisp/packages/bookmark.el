(require 'bookmark)

(customize-set-variable
 'bookmark-default-file
 (concat bkalmar/emacs-config-directory "bookmark/bookmarks"))
(mkdir (file-name-directory bookmark-default-file) t)
