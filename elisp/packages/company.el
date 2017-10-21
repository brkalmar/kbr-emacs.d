(require 'company)

;;; customization

(customize-set-variable 'company-idle-delay 0.25)
(customize-set-variable 'company-minimum-prefix-length 2)
(customize-set-variable 'company-show-numbers t)
(customize-set-variable 'company-tooltip-limit 20)

;;; backends

(add-to-list 'company-backends 'company-nxml)
(add-to-list 'company-backends 'company-css)
(add-to-list 'company-backends 'company-clang)
(add-to-list 'company-backends 'company-files)

(require 'company-auctex)
(company-auctex-init)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(require 'company-ghci)
(add-to-list 'company-backends 'company-ghci)

(require 'company-irony)
(add-to-list 'company-backends 'company-irony)

(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(require 'company-math)
(add-to-list 'company-backends 'company-latex-commands)
(add-to-list 'company-backends 'company-math-symbols-latex)
(add-to-list 'company-backends 'company-math-symbols-unicode)

(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)

;; enable
(add-hook 'after-init-hook 'global-company-mode)
