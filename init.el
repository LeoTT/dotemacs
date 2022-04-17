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

(require 'package)
(add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.org/packages/")
   t)

    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (eval-when-compile
      (require 'use-package))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'tango-dark t)

(custom-set-faces
'(default ((t (:family "Fira Code" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

(setq ring-bell-function 'ignore)

(tool-bar-mode -1)

(scroll-bar-mode 0)

(when (not (display-graphic-p))
  (menu-bar-mode -1))

  (use-package imenu-list
    :ensure t)

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode t))

  (use-package markdown-mode
    :ensure t
    :mode "\\.md$")

(use-package diminish
:ensure t)
(use-package bind-key
:ensure t)

(when (memq window-system '(mac ns))
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-right-command-modifier 'alt
        ns-transparent-titlebar t))

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

(use-package org-ref
  :ensure t)
(require 'org-ref)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-preserve-indentation t)
   (defun org-mode-startup ()
     (org-content 3))

   (add-hook 'org-mode-hook 'org-mode-startup)

   (setq org-src-fontify-natively t)
   (use-package auctex
     :ensure t
     :defer t
     :config
     (defvar ispell-program-name)
     (setq ispell-program-name "/usr/local/bin/aspell"
           TeX-PDF-mode t
           TeX-auto-save t
           TeX-parse-self t)
     (ispell-change-dictionary "de" t)
     (add-hook 'LaTeX-mode-hook 'tex-fold-mode)
     (flyspell-mode 1))

   (require 'ox-latex)
   (unless (boundp 'org-latex-classes)
     (setq org-latex-classes nil))
   (add-to-list 'org-latex-classes
                '("myarticle" "\\documentclass[11pt]{myarticle}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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
        '((company-mode              " cmpy")
          (eldoc-mode                " doc"))))

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1))

(setq explicit-shell-file-name "/bin/bash")

     (setq standard-indent 2
           tab-width 2)
     (setq-default indent-tabs-mode nil
                   tab-width 2)

     ;; (use-package indent-guide
     ;;   :ensure t
     ;;   :config (indent-guide-global-mode 1))

     (use-package yasnippet
       :ensure t
       :config (yas-global-mode 1))

;;(load "~/.emacs.d/org-show/org-show")
;;(require 'org-show)

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

(use-package hippie-exp
  :ensure t
  :defer t
  :bind (("M-ä" . hippie-expand)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package magit-gitflow
  :ensure t)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

     (setq backup-directory-alist `(("." . "~/.saves"))
           backup-by-copying t)

     (use-package nov
       :ensure t
     )

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

  (defun overwrite-keys (keypairs)
    (dolist (keypair keypairs)
      (let ((old-key (car keypair))
            (new-key (cdr keypair)))
            (define-key key-translation-map (kbd old-key) (kbd new-key)))))

  (global-set-key (kbd "<s-up>") 'windmove-up)
  (global-set-key (kbd "<s-left>") 'windmove-left)
  (global-set-key (kbd "<s-down>") 'windmove-down)
  (global-set-key (kbd "<s-right>") 'windmove-right)

  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode t)
    (key-chord-define-global "z7" (lambda () (interactive) (insert "/")))
    ;; (key-chord-define-global "88" (lambda () (interactive) (insert ")")))
    ;; (key-chord-define-global "99" (lambda () (interactive) (insert "}")))
)

  (when (memq window-system '(mac ns))
    (overwrite-keys '(("§" . "&")
                      ("6" . "6")
                      ("&" . "/")
                      ("/" . "[")
                      ("ß" . "?")
                      ("?" . "ß")
                      ("s-5" . "[")
                      ("s-6" . "]")
                      ("s-7" . "|")
                      ("s-S-7" . "\\")
                      ("s-8" . "{")
                      ("s-9" . "}")
                      ("s-l" . "@")
                      ("s-/" . "\\")
                      ("s-n" . "~"))))

  (global-set-key (kbd "C-^") 'toggle-frame-maximized)

(use-package flycheck
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    ;; (flycheck-add-mode 'typescript-tslint 'typescript-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint))

                  flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist))

                  ;; flycheck-disabled-checkers
                  ;; (append flycheck-disabled-checkers
                  ;;         '(typescript-tide))

                  flycheck-temp-prefix ".flycheck")
    (global-flycheck-mode 1)))

(use-package yasnippet
:ensure t
:config
(yas-global-mode 1))

(defun init-presentation ()
  (interactive)
  (shell-command "wget https://github.com/hakimel/reveal.js/archive/master.tar.gz")
  (shell-command "tar -xzvf master.tar.gz")
  (shell-command "Mv reveal.js-master reveal.js"))
(use-package ox-pandoc
  :ensure t)
;;(require 'ox-pandoc)
;;(require 'org)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (awk . t)
     (calc .t)
     (C . t)
     (emacs-lisp . t)
     (haskell . t)
     (gnuplot . t)
     (latex . t)
     ;;(ledger . t)
     (js . t)
     (haskell . t)
     (perl . t)
     (python . t)
     ;; (gnuplot . t)
     (shell . t)))

  (defun open-init-org ()
      (interactive)
    (find-file-existing "~/.emacs.d/init.org"))

(defun quick-shell ()
    (interactive)
  (shell (concat "**" default-directory "**")))

  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)))

  (use-package haskell-mode
    :ensure t
    :mode "\\.hs$"
    :config
    (add-hook 'haskell-mode-hook 'prettify-symbols-mode)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
                )))

  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)))

       (add-hook 'emacs-lisp-mode-hook
                 '(lambda () (progn
                               (prettify-symbols-mode t)
                               (show-paren-mode t)
                               (electric-pair-mode t))))

       (use-package rainbow-delimiters
         :ensure t
         :init
         (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
         (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

  (use-package company-jedi
    :ensure t
    :config
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))

    (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package add-node-modules-path
  :ensure t)

(defvar js-ts-prettify-symbols-alist
  '(("<=" . ?≤)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("public" . ?+)
    ("private" . ?-)
    ("async" . ?⌚)
    ("await" . ?⌚)
    (">=" . ?≥)
    ;; ("=>" . ?⇒)
    ("return" . ?↳)
    ("!==" . ?≠)))

     (use-package js2-mode
       :ensure t
       :defer 1
       :mode "\\.js$"
       :config
       (add-hook 'js2-mode-hook 'prettify-symbols-mode)
       (add-hook 'js2-mode-hook
                 (lambda ()
                   (setq-local prettify-symbols-alist js-ts-prettify-symbols-alist)
                   ))
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

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"  "\\.css\\'" "\\.svelte\\'" "\\.tsx\\'")
  :interpreter "web"
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        css-indent-offset 2)
  ;; (when (string= (file-name-extension buffer-file-name) "tsx")
  ;;   (setup-tide-mode))
  )

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :init
    (add-hook 'web-mode-hook #'emmet-mode)
  :config (when (and (stringp buffer-file-name)
                 (string-match "\\.css\\'" buffer-file-name))
            (setq emmet-use-css-transform t)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config (lsp-enable-which-key-integration t))

;; (defun setup-tide-mode()
;;   (interactive)
;;   (tide-setup)
;;     ;; (flycheck-mode +1)
;;     ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     (company-mode +1))

;; (use-package tide
;;   :ensure t
;;   :defer 1
;;   :bind (("C-c <up>" . tide-jump-to-definition))
;;   :config
;;     ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;     ;; (add-hook 'js2-mode-hook #'setup-tide-mode)
;;     ;; (add-hook 'web-mode-hook #'setup-tide-mode)
;;     (flycheck-add-next-checker 'typescript-tide '(t . javascript-eslint) 'append)
;;     (flycheck-add-next-checker 'javascript-tide '(t . javascript-eslint) 'append)
;;     (setq tide-format-options '(
;;                             :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
;;                             :placeOpenBraceOnNewLineForFunctions nil)))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.ts\\'" "\\.jsx\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'prettify-symbols-mode)
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local prettify-symbols-alist js-ts-prettify-symbols-alist)
              )))


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

  (use-package geiser
    :ensure t
    :config (setq geiser-scheme-implementation 'guile)
    :bind ("C-c C-h" . geiser-doc-symbol-at-point))

  (use-package scheme-complete
    :ensure t)

  (defvar hy-prettify-symbols-alist
    '(("fn" . ?ƒ)
      ("->" . ?→)))

  (use-package hy-mode
    :ensure t
    :mode ("\\.hy\\'")
    :config
    (add-hook 'hy-mode-hook 'prettify-symbols-mode)
    (add-hook 'hy-mode-hook
              (lambda ()
                (setq-local prettify-symbols-alist hy-prettify-symbols-alist)
                )))

  (use-package cider
    :ensure t)

  (use-package clojure-mode
    :ensure t
    :mode ("\\.clj\\'"))

  (load "./prolog.el")
  (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
  (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
  (setq prolog-system 'swi)  ; optional, the system you are using;
                                          ; see `prolog-system' below for possible values
  (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                  ("\\.m$" . mercury-mode))
                                 auto-mode-alist))
  (eval-after-load 'prolog
                    '(define-key prolog-mode-map (kbd "C-x C-e") 'ediprolog-dwim))
  (use-package ediprolog
      :ensure t
      :config
      (setq ediprolog-system 'swi))

    (use-package omnisharp
      :ensure t
      :config
      (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
      (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)

    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    ;; (c-set-style "ellemtel")
    ;; (setq c-basic-offset 4)
    ;; (setq truncate-lines t)
    ;; (setq tab-width 4)

    ;csharp-mode README.md recommends this too
    ;(electric-pair-mode 1)       ;; Emacs 24
    ;(electric-pair-local-mode 1) ;; Emacs 25

    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile))

  (eval-after-load
   'company
   '(add-to-list 'company-backends 'company-omnisharp))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (reason-mode . lsp)
;;   :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; (use-package company-lsp
;;   :ensure t
;;   :after company lsp-mode
;;   :init
;;   (push 'company-lsp company-backends))

(use-package reason-mode
  :ensure t
  :after lsp-mode
  :mode ("\\.ml\\'" . reason-mode)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "~/Downloads/rls-macos/reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls)))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs$")

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))
  (defun psc-ide-ensure ()
    (interactive)
    (let ((prj (projectile-project-root)))
      (progn
        (setq psc-ide-current prj)
        (psc-ide-server-start-impl (expand-file-name prj))
        (sit-for 3) ;; waiting for the server to start to send it commands
        (psc-ide-load-all)
        (message (format "psc-ide started for %s" (projectile-project-name)))))))

(setq cds-highlights
      '(("entity" . 'font-lock-function-name-face)
        ("managed" . 'font-lock-constant-face)))

(define-derived-mode cds-mode fundamental-mode "cds"
  "major mode for editing mymath language code."
  (setq font-lock-defaults '(cds-highlights)))

(defun concat-paths (dirs)
  (if (null dirs)
      ""
    (concat (file-name-as-directory (car dirs)) (concat-paths (cdr dirs)))))



(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(cds-mode . "cds"))
  (let* ((node_modules-path (replace-regexp-in-string "\n$" ""
                                                      (shell-command-to-string "npm root -g")))

         (server-path (concat (concat-paths `(,node_modules-path
                                              "@sap"
                                              "cds-lsp"
                                              "dist")) "main.js"))
         (server-command `("node" ,server-path "--stdio")))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection server-command)
                      :activation-fn (lsp-activate-on "cds")
                      :server-id 'cds-ls))))
