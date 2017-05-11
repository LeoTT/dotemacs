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

;;(ido-vertical-mode 1)


;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets") 
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        "/path/to/yasnippet/snippets"         ;; the default collection
        ))

;; Flycheck
(use-package flycheck
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
                               flycheck-temp-prefix ".flycheck")
                 (global-flycheck-mode 1)
                 ))



(use-package company-mode

  :config
;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))



