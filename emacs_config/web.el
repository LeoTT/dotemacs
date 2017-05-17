(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"  "\\.css\\'")
  :interpreter "web")

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :init 
  (progn
    (add-hook 'web-mode-hook #'emmet-mode)))
