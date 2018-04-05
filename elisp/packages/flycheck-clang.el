(require 'flycheck)

(add-to-list 'flycheck-clang-warnings "pedantic")

(add-hook 'c-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c11")))
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11")))
