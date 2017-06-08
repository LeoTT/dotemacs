(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq explicit-shell-file-name "/bin/bash")

;;; indentation
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)

(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
