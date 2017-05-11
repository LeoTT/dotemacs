(use-package web-mode
  :mode ("\\.html\\'"  "\\.css\\'")
  :interpreter "web")

(use-package emmet-mode
  :commands (emmet-mode)
  :init 
  (progn
    (add-hook 'web-mode-hook #'emmet-mode)))
