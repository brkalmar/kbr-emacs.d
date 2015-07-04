;; Custom keybindings.
;; `C-c [A-Za-z]' is reserved for users.
;;
;; 2015  Bence Kalmar

(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)
(global-set-key (kbd "C-c e") 'bkalmar/terminal)
(global-set-key (kbd "C-c f") 'toggle-frame-maximized)
(global-set-key (kbd "C-c h") 'bkalmar/enable-themes)
(global-set-key (kbd "C-c H") 'bkalmar/disable-themes)
(global-set-key (kbd "C-c i") 'bkalmar/insert-copyright-comment)
(global-set-key (kbd "C-c l") 'fill-region)
(global-set-key (kbd "C-c L") 'fill-region-as-paragraph)
(global-set-key (kbd "C-c m") 'normal-mode)
(global-set-key (kbd "C-c q") 'insert-char)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'replace-regexp)
(global-set-key (kbd "C-c t") 'bkalmar/temp-buffer)
(global-set-key (kbd "C-c v") 'global-auto-revert-mode)
(global-set-key (kbd "C-c w") 'bkalmar/wordprocessor-mode)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; semantic-mode
(global-set-key (kbd "C-c d") 'semantic-ia-show-doc)
(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c o") 'senator-fold-tag)
(global-set-key (kbd "C-c O") 'senator-unfold-tag)

;; org
(global-set-key (kbd "C-c g") 'org-agenda)
(global-set-key (kbd "C-c n") 'bkalmar/find-org-default-notes-file)
;; `C-c ,` as described in the manual doesn't work
(global-set-key (kbd "C-c p") 'org-priority)
(global-set-key (kbd "C-c u") 'org-capture)

;; window
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)
