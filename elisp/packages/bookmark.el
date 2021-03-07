(require 'bookmark)

(customize-set-variable
 'bookmark-default-file
 (concat kbr/emacs-config-directory "bookmark/bookmarks"))
(mkdir (file-name-directory bookmark-default-file) t)
