(require 'company)

;;; customization

(customize-set-variable 'company-idle-delay 0.25)
(customize-set-variable 'company-minimum-prefix-length 2)
(customize-set-variable 'company-show-numbers t)
(customize-set-variable 'company-tooltip-limit 20)

;;; backends

(require 'company-auctex)
(company-auctex-init)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)

;; enable
(global-company-mode +1)
