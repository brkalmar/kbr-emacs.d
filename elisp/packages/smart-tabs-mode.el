(require 'map)
(require 'smart-tabs-mode)

(defvar bkalmar/smart-tabs-languages
  '(c c++)
  "All languages `smart-tabs-mode' is enabled for.")

(map-keys-apply
 'smart-tabs-insinuate
 (map-filter (lambda (lang def) (memq lang bkalmar/smart-tabs-languages))
             smart-tabs-insinuate-alist))
