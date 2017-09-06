(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-right-command-modifier 'alt)



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

(define-key key-translation-map (kbd "<dead-acute>") (kbd "`"))
(define-key key-translation-map (kbd "ö") (kbd ";"))
(define-key key-translation-map (kbd "ä") (kbd ":"))
(define-key key-translation-map (kbd ";") (kbd "Ö"))
(define-key key-translation-map (kbd ":") (kbd "Ä"))
(define-key key-translation-map (kbd "Ö") (kbd "ö"))
(define-key key-translation-map (kbd "Ä") (kbd "ä"))
(define-key key-translation-map (kbd "#") (kbd "'"))
(define-key key-translation-map (kbd "'") (kbd "#"))

(when (memq window-system '(mac ns))
  (define-key key-translation-map (kbd "A-1") (kbd "1"))
  (define-key key-translation-map (kbd "A-2") (kbd "2"))
  (define-key key-translation-map (kbd "A-3") (kbd "3"))
  (define-key key-translation-map (kbd "A-4") (kbd "4"))

  (define-key key-translation-map (kbd "A-<") (kbd "0"))
  (define-key key-translation-map (kbd "A-y") (kbd "1"))
  (define-key key-translation-map (kbd "A-x") (kbd "2"))
  (define-key key-translation-map (kbd "A-c") (kbd "3"))
  (define-key key-translation-map (kbd "A-a") (kbd "4"))
  (define-key key-translation-map (kbd "A-s") (kbd "5"))
  (define-key key-translation-map (kbd "A-d") (kbd "6"))
  (define-key key-translation-map (kbd "A-q") (kbd "7"))
  (define-key key-translation-map (kbd "A-w") (kbd "8"))
  (define-key key-translation-map (kbd "A-e") (kbd "9"))

  (define-key key-translation-map (kbd "§") (kbd "&"))

  (define-key key-translation-map (kbd "6") (kbd "/"))
  (define-key key-translation-map (kbd "&") (kbd "\\"))

  (define-key key-translation-map (kbd "7") (kbd "["))
  (define-key key-translation-map (kbd "/") (kbd "]"))

  (define-key key-translation-map (kbd "8") (kbd "("))
  (define-key key-translation-map (kbd "(") (kbd ")"))

  (define-key key-translation-map (kbd "9") (kbd "{"))
  (define-key key-translation-map (kbd ")") (kbd "}"))

  (define-key key-translation-map (kbd "ß") (kbd "?"))
  (define-key key-translation-map (kbd "?") (kbd "ß"))
)
;; (global-set-key (kbd "s-d") 'mc/mark-next-like-this-word)

(global-set-key (kbd "C-S-n") 'copy-line)
(global-set-key (kbd "M-ö") 'dirgrep)


(global-set-key (kbd "C-^") 'toggle-frame-maximized)

(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-right>") 'windmove-right)
