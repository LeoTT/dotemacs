
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'tron t)

(setq ring-bell-function 'ignore)

(tool-bar-mode -1)

(when (memq window-system '(mac ns))
       (setq mac-option-modifier 'super
             mac-command-modifier 'meta
             ns-right-command-modifier 'alt))

     (use-package exec-path-from-shell
       :ensure t
       :config (exec-path-from-shell-initialize))

(exec-path-from-shell-initialize)

(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)
(defadvice dired-mark-read-file-name (after rv:dired-create-dir-when-needed (prompt dir op-symbol arg files &optional default) activate)
  (when (member op-symbol '(copy move))
    (let ((directory-name (if (< 1 (length files))
                              ad-return-value
                              (file-name-directory ad-return-value))))
      (when (and (not (file-directory-p directory-name))
                 (y-or-n-p (format "directory %s doesn't exist, create it?" directory-name)))
        (make-directory directory-name t)))))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))
(add-hook 'after-save-hook 'tangle-init)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq org-src-fontify-natively t)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(use-package dim
  :ensure t
  :config
  (dim-major-names
   '((emacs-lisp-mode           "EL")
     (inferior-emacs-lisp-mode  "EL>")
     (typescript-mode           "TS")
     (calendar-mode             "📆")))
  (dim-minor-names
   '((tide-mode                 " ti")
     (company-mode              " cmpy")
     (eldoc-mode                " doc"))))

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1))

(setq explicit-shell-file-name "/bin/bash")

(setq standard-indent 2
      tab-width 2)
(setq-default indent-tabs-mode nil
              tab-width 2)

(use-package indent-guide
  :ensure t
  :config (indent-guide-global-mode 1))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-ü" . ace-jump-mode)
         ("<f9>" . ace-jump-mode)
         ("<f12>" . ace-jump-mode)
         ("<f8>" . ace-jump-char-mode)))
(use-package ace-window
  :ensure t
  :bind (("M-ü" . ace-window)
         ("A-ü" . ace-window)))
(use-package helm
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("M-ö" . helm-ag)))
(use-package ido
  :ensure t
  :config (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1))

(use-package smex
  :ensure t
  :config (global-set-key (kbd "M-x") 'smex))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t)

(use-package nov
  :ensure t
)

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

(add-hook 'emacs-lisp-mode-hook
          '(lambda () (progn
                        (show-paren-mode t)
                        (electric-pair-mode t))))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"  "\\.css\\'" "\\.tsx\\'" "\\.jsx\\'")
  :interpreter "web"
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :init
    (add-hook 'web-mode-hook #'emmet-mode)
  :config (when (and (stringp buffer-file-name)
                 (string-match "\\.css\\'" buffer-file-name))
            (setq emmet-use-css-transform t)))

(use-package js2-mode
  :ensure t
  :defer 1
  :mode "\\.js$"
  :config
    (add-hook 'js2-mode-hook 'prettify-symbols-mode)
    (add-hook 'js2-mode-hook
              (lambda ()
                'prettify-symbols-mode
                (push '("<=" . ?≤) prettify-symbols-alist)
                (push '(">=" . ?≥) prettify-symbols-alist)
                (push '("=>" . ?⟹) prettify-symbols-alist)
                (push '("!==" . ?≠) prettify-symbols-alist)))
    (font-lock-add-keywords 'js2-mode
                            '(("require" . font-lock-keyword-face)))
    (setq
     js-indent-level 2
     js2-basic-offset 2
     js2-bounce-indent-p t
     js2-strict-missing-semi-warning nil
     js2-concat-multiline-strings nil
     js2-include-node-externs t
     js2-skip-preprocessor-directives t
     js2-strict-inconsistent-return-warning nil))

(use-package indium
  :ensure t)

(defun is-current-line-end-of-function ()
    "returns nil if current line is end of a function expression"
    (string-match-p ")\\(: [^ ]*\\)?\\ \\(=> \\)?{" (thing-at-point 'line t)))

     (use-package tide
       :ensure t
       :defer 1
       :bind (("C-c <up>" . tide-jump-to-definition))
       :config
       (progn
         (add-hook 'typescript-mode-hook #'setup-tide-mode)
         (add-hook 'js2-mode-hook #'setup-tide-mode)
         (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
         (setq tide-format-options '(
                                 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                 :placeOpenBraceOnNewLineForFunctions nil))))

     (use-package typescript-mode
       :ensure t
       :mode "\\.ts$"
       :config
       (setq typescript-indent-level 2)
       (add-hook 'typescript-mode-hook 'prettify-symbols-mode)
       (add-hook 'typescript-mode-hook
                 (lambda ()
                   'prettify-symbols-mode
                   (push '("<=" . ?≤) prettify-symbols-alist)
                   (push '(">=" . ?≥) prettify-symbols-alist)
                   (push '("=>" . ?⟹) prettify-symbols-alist)
                   (push '("!==" . ?≠) prettify-symbols-alist)))
       (defun typescript--proper-indentation (parse-status)
         "Overwriting original function in order to fix multiparam/newline indentation"
         (save-excursion
           (back-to-indentation)
           (cond ((nth 4 parse-status)
                  (typescript--get-c-offset 'c (nth 8 parse-status)))
                 ((nth 8 parse-status) 0) ; inside string
                 ((typescript--ctrl-statement-indentation))
                 ((eq (char-after) ?#) 0)
                 ((save-excursion (typescript--beginning-of-macro)) 4)
                 ((nth 1 parse-status)
                  (let ((same-indent-p (looking-at
                                        "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                        (continued-expr-p (typescript--continued-expression-p)))
                    (goto-char (nth 1 parse-status))
                    (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                        (progn
                          (skip-syntax-backward " ")
                          (when (eq (char-before) ?\)) (backward-list))


                          (if (is-current-line-end-of-function)
                              (unless (string-match-p "[(]" (thing-at-point 'line t))
                              (condition-case nil
                                  (re-search-backward "[(]")
                                (error nil))))

                          (back-to-indentation)
                          (cond (same-indent-p
                                 (current-column))
                                (continued-expr-p
                                 (+ (current-column) (* 2 typescript-indent-level)
                                    typescript-expr-indent-offset))
                                (t
                                 (+ (current-column) typescript-indent-level))))
                      (unless same-indent-p
                        (forward-char)
                        (skip-chars-forward " \t"))
                      (current-column))))
                 ((typescript--continued-expression-p)
                  (+ typescript-indent-level typescript-expr-indent-offset))
                 (t 0)))))
     (defun setup-tide-mode()
       (interactive)
       (tide-setup)
         ;; (flycheck-mode +1)
         ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
         (eldoc-mode +1)
         (tide-hl-identifier-mode +1)
         (company-mode +1))
     (defun next-import ()
       (condition-case nil
           (progn
             (re-search-forward "^import.*from.*$")
             (move-beginning-of-line 1))
         (error
          (goto-char (point-max)))))

     (defun import-start-key ()
       (search-forward "'" nil nil)
       ;; find  a better way to return nil
       (quote nil))

     (defun import-sort ()
         "Typescript/ES6 import sort"
         (interactive)
         (save-excursion
           (goto-char (point-min))
           (next-import)
               (sort-subr nil 'next-import 'end-of-line 'import-start-key 'import-start-key)))


