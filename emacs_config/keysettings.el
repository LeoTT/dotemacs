
(global-set-key [C-tab] 'set-rectangular-region-anchor)

;; (global-set-key (kbd "M-x") 'smex)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
  
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-ü") 'ace-jump-mode)
;; (global-set-key (kbd "M-ü") 'ace-window)

(global-set-key (kbd "s-5") nil)
(define-key key-translation-map (kbd "s-5") (kbd "["))
(global-set-key (kbd "s-6") nil)
(define-key key-translation-map (kbd "s-6") (kbd "]"))
(global-set-key (kbd "s-7") nil)
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(global-set-key (kbd "s-S-7") nil)
(define-key key-translation-map (kbd "s-S-7") (kbd "\\"))
(global-set-key (kbd "s-8") nil)
(define-key key-translation-map (kbd "s-8") (kbd "{"))
(global-set-key (kbd "s-9") nil)
(define-key key-translation-map (kbd "s-9") (kbd "}")) 
(global-set-key (kbd "s-l") nil)
(define-key key-translation-map (kbd "s-l") (kbd "@"))
(global-set-key (kbd "s-/") nil)
(define-key key-translation-map (kbd "s-/") (kbd "\\"))
(global-set-key (kbd "s-n") nil)
(define-key key-translation-map (kbd "s-n") (kbd "~"))

;; (require 'multiple-cursors)		
(global-set-key (kbd "s-d") 'mc/mark-next-like-this-word)

(global-set-key (kbd "C-S-n") 'copy-line)
(global-set-key (kbd "M-ö") 'dirgrep)


