(use-package js2-mode
  :defer 1
  :mode "\\.js$"
  :config
  (progn
    (add-hook 'js2-mode-hook 'prettify-symbols-mode)
    (add-hook 'js2-mode-hook
              (lambda ()
                'prettify-symbols-mode            
                (push '("<=" . ?≤) prettify-symbols-alist)
                (push '(">=" . ?≥) prettify-symbols-alist)
                (push '("=>" . ?⟹) prettify-symbols-alist)
                (push '("!==" . ?≠) prettify-symbols-alist)))
    (setq
     js-indent-level 2
     js2-basic-offset 2
     js2-bounce-indent-p t
     js2-strict-missing-semi-warning nil
     js2-concat-multiline-strings nil
     js2-include-node-externs t
     js2-skip-preprocessor-directives t
     js2-strict-inconsistent-return-warning nil)))


(use-package typescript-mode
  :mode "\\.ts$"
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :defer 1
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
   
    (setq tide-format-options '(
                            :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                            :placeOpenBraceOnNewLineForFunctions nil))))

(setq tide_disabled nil)

(defun disable-tide()
       (interactive)
       (setq tide_disabled t))

(defun enable-tide()
       (interactive)
       (setq tide_disabled nil))

(defun setup-tide-mode()
  (interactive)
  (unless tide_disabled
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    
    ;; company is an optional dependency.You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  )
