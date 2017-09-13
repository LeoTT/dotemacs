(setq ring-bell-function 'ignore)

(use-package dim
  :ensure t
  :config(progn
           (dim-major-names
            '((emacs-lisp-mode           "EL")
              (inferior-emacs-lisp-mode  "EL>")
              (typescript-mode           "TS")
              (calendar-mode             "📆")))
           (dim-minor-names
            '((tide-mode                 " ti")
              (company-mode              " cmpy")
              (eldoc-mode                " doc")))))

(use-package emojify
  :ensure t)

(use-package auctex
  :ensure t
  :defer t
  :config(progn
           (setq ispell-program-name "/usr/local/bin/aspell")
           (setq TeX-PDF-mode t)
           (setq TeX-auto-save t)
           (setq TeX-parse-self t)))

(use-package hippie-exp
  :ensure t
  :defer t
  :bind (("M-ä" . hippie-expand)))



;; (use-package god-mode
;;   :ensure t
;;   :defer t
;;   :config(progn
;;            (global-set-key (kbd "<escape>") 'god-local-mode)
;;            (define-key key-translation-map (kbd "6") (kbd "/"))
;;            (defun weird-numbers ()
;;              (progn (define-key key-translation-map (kbd "6") (kbd "/"))
;;                     (define-key key-translation-map (kbd "&") (kbd "\\"))

;;                     (define-key key-translation-map (kbd "7") (kbd "["))
;;                     (define-key key-translation-map (kbd "/") (kbd "]"))

;;                     (define-key key-translation-map (kbd "8") (kbd "("))
;;                     (define-key key-translation-map (kbd "(") (kbd ")"))

;;                     (define-key key-translation-map (kbd "9") (kbd "{"))
;;                     (define-key key-translation-map (kbd ")") (kbd "}"))

;;                     )
;;              )
;;            (defun original-numbers ()
;;              (progn (define-key key-translation-map (kbd "6") (kbd "6"))
;;                     (define-key key-translation-map (kbd "&") (kbd "&"))

;;                     (define-key key-translation-map (kbd "7") (kbd "7"))
;;                     (define-key key-translation-map (kbd "/") (kbd "/"))

;;                     (define-key key-translation-map (kbd "8") (kbd "8"))
;;                     (define-key key-translation-map (kbd "(") (kbd "("))

;;                     (define-key key-translation-map (kbd "9") (kbd "9"))
;;                     (define-key key-translation-map (kbd ")") (kbd ")"))

;;                     )
;;              )
;;            (defun my-update-cursor ()
;;              (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                                    'box
;;                                  'bar)))

;;            (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;;            (add-hook 'god-mode-disabled-hook 'my-update-cursor)

;;            (add-hook 'god-mode-enabled-hook 'original-numbers)
;;            (add-hook 'god-mode-disabled-hook 'weird-numbers)

;;            (defun c/god-mode-update-cursor ()
;;              (let ((limited-colors-p (> 257 (length (defined-colors)))))
;;                (cond (god-local-mode (progn
;;                                        (set-face-background 'mode-line "red")
;;                                        (set-face-background 'mode-line-inactive "black")))
;;                      (t (progn
;;                           (set-face-background 'mode-line "green")
;;                           (set-face-background 'mode-line-inactive "black")))))
;;            )
;;   ))

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1))


(use-package magit-gitflow
  :ensure t)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-c C-c M-x" . execute-extended-command)
         ("M-n" . smex-major-mode-commands))
  :config (smex-initialize))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-ü" . ace-jump-mode)))

(use-package ace-window
  :ensure t
  :bind (("M-ü" . ace-window)
         ("A-ü" . ace-window)))

(use-package ido
  :ensure t
  :config (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1))

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'typescript-tslint 'typescript-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint))

                  flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist))

                  ;; flycheck-disabled-checkers
                  ;; (append flycheck-disabled-checkers
                  ;;         '(typescript-tide))

                  flycheck-temp-prefix ".flycheck")
    (global-flycheck-mode 1)
    ))



(use-package company
  :ensure t
  :config
;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))



(use-package eldoc
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("M-ö" . helm-ag)))
