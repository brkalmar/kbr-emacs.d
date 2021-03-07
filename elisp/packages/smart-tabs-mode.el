(require 'smart-tabs-mode)
(require 'map)

(defvar kbr/smart-tabs-languages
  '(c c++)
  "All languages `smart-tabs-mode' is enabled for.")

(map-keys-apply
 'smart-tabs-insinuate
 (map-filter (lambda (lang def) (memq lang kbr/smart-tabs-languages))
             smart-tabs-insinuate-alist))
