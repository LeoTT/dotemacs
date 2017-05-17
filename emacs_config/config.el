


(setq explicit-shell-file-name "/bin/bash")

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; (add-hook 'eshell-preoutput-filter-functions
;;           'ansi-color-filter-apply)


;;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
;;(setq js-indent-level 2)

(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
