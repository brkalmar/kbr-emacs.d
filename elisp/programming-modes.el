;; Programming mode customizations
;;
;; 2015  Bence Kalmar

;; NOTE: fci-mode temporarily removed because of incompatibility with web-mode

(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'text-mode-hook 'linum-mode t)

(add-hook 'prog-mode-hook 'whitespace-mode t)

(add-hook 'prog-mode-hook 'form-feed-mode t)
(add-hook 'text-mode-hook 'form-feed-mode t)

;; (add-hook 'prog-mode-hook 'hs-minor-mode t)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'glasses-mode)
(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (push '("->" . ?→) prettify-symbols-alist)
              (prettify-symbols-mode))))
