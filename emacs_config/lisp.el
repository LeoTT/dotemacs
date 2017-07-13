

(use-package emacs-lisp-mode
  :init
  (progn
    (show-paren-mode t)
    (use-package rainbow-delimiters
      :ensure t
      :init
      (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))))
