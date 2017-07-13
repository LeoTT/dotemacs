(use-package js2-mode
  :ensure t
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
     js2-strict-inconsistent-return-warning nil)))

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






(use-package tide
  :ensure t
  :defer 1
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
    (setq tide-format-options '(
                            :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                            :placeOpenBraceOnNewLineForFunctions nil))))

(use-package indium
  :ensure t)

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
    ;; (flycheck-mode +1)
    ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)

    ;; company is an optional dependency.You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  )

(defun is-current-line-end-of-function ()
    "returns nil if current line is end of a function expression"
    (string-match-p ")\\(: [^ ]*\\)?\\ \\(=> \\)?{" (thing-at-point 'line t)))

(defun aurelia-inject ()
  "Prompt user to enter a string, with input history support."
  (interactive)
  (let ((module-name (read-string "Enter modulename:"))
        (module-path (read-string "Enter relative path")))

        (message "String is %s." module-name)))
