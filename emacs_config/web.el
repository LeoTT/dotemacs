(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"  "\\.css\\'")
  :interpreter "web"
  :config
  (progn
    (add-hook 'web-mode-hook 'prettify-symbols-mode)
    (add-hook 'web-mode-hook
              (lambda ()
                'prettify-symbols-mode
                (push '("<h2>" . "asda") prettify-symbols-alist)
                (push '(">=" . ?≥) prettify-symbols-alist)
                (push '("=>" . ?⟹) prettify-symbols-alist)
                (push '("!==" . ?≠) prettify-symbols-alist)))
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-markup-indent-offset 2)))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :init
  (progn
    (add-hook 'web-mode-hook #'emmet-mode)))
