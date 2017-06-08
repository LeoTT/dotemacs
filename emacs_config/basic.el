(setq ring-bell-function 'ignore)


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
  :bind (("M-ü" . ace-window)))

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
