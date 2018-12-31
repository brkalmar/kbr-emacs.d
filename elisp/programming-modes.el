;; Programming mode customizations
;;
;; 2015  Bence Kalmar

(require 'zenburn-theme)

;; NOTE: fci-mode temporarily removed because of incompatibility with web-mode

(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'text-mode-hook 'linum-mode t)

(add-hook 'prog-mode-hook 'eldoc-mode)

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
              (push '("!=" . ?≠) prettify-symbols-alist)
              (push '("->" . ?→) prettify-symbols-alist)
              (push '("::" . ?∷) prettify-symbols-alist)
              (push '("<=" . ?≤) prettify-symbols-alist)
              (push '("==" . ?═) prettify-symbols-alist)
              (push '("=>" . ?⇒) prettify-symbols-alist)
              (push '(">=" . ?≥) prettify-symbols-alist)
              (prettify-symbols-mode))))

;;; Highlight uppercase comment keywords TODO/NOTE/etc.
(defface bkalmar/comment-uppercase-keyword-face
  '((t . (:underline t)))
  "Common face for all uppercase keywords in comments.")
(zenburn-with-color-variables
  (defface bkalmar/comment-todo-face
    `((t . (:inherit bkalmar/comment-uppercase-keyword-face
                     :foreground ,zenburn-red+2
                     :weight bold)))
    "Highlights TODO in comments.")
  (defface bkalmar/comment-note-face
    `((t . (:inherit bkalmar/comment-uppercase-keyword-face
                     :foreground ,zenburn-yellow)))
    "Highlights NOTE in comments."))
(add-hook
 'prog-mode-hook
 (lambda () (font-lock-add-keywords
        nil
        '(("\\<\\(TODO\\):" . (1 'bkalmar/comment-todo-face prepend))
          ("\\<\\(NOTE\\):" . (1 'bkalmar/comment-note-face prepend))))))

;; keybindings

(define-key prog-mode-map (kbd "<C-tab>") 'company-complete-common)
