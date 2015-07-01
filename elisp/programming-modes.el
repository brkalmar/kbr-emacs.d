;; Programming mode customizations
;;
;; 2015  Bence Kalmar

;; NOTE: fci-mode temporarily removed because of incompatibility with
;; auto-complete & web-mode

(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'text-mode-hook 'linum-mode t)

(add-hook 'prog-mode-hook 'form-feed-mode t)
(add-hook 'text-mode-hook 'form-feed-mode t)

;; (add-hook 'prog-mode-hook 'hs-minor-mode t)

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
