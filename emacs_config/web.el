(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"  "\\.css\\'")
  :interpreter "web"
  :config
  (progn
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-markup-indent-offset 2)))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :init
  (progn
    (add-hook 'web-mode-hook #'emmet-mode)))
