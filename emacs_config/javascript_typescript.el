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
    (company-mode +1))
  )

(defun is-current-line-end-of-function ()
    "returns nil if current line is end of a function expression"
    (string-match-p ")\\(: [^ ]*\\)?\\ \\(=> \\)?{" (thing-at-point 'line t)))

(defun aurelia-inject ()
  "Prompt user to enter a string, with input history support."
  (interactive)
  (progn
    (catch 'aurelia-inject-exit

      (buffer-contains-class)

      (let ((import-name (read-string "Enter classname: "))
            (module-path (read-string "Enter modulename or relative path: "))
            (var-name (read-string "Enter instance var name:  ")))
        (save-excursion
          (goto-char (point-min))
          (insert (format "import {%s} from '%s';\n" import-name module-path))

          (unless (ignore-errors (search-forward-regexp " *import +{ *\\(\\w*, *\\)*inject *\\(, *\\w* *\\)* *} *from *'aurelia-framework';"))
            (if (ignore-errors (search-forward-regexp " *import +{[a-zA-Z ,]*} *from *'aurelia-framework';"))
                (progn
                  (search-backward "{")
                  (forward-char)
                  (insert "inject, "))
              (insert "import {inject} from 'aurelia-framework'\n;"))

            (if (ignore-errors (search-forward-regexp "@inject([a-zA-Z ,]*)"))
                (progn
                  (search-backward "(")
                  (forward-char)
                  (insert (format "%s, " import-name)))
              (progn
                (search-forward-regexp "\\(export\\)? *class [A-Za-z] *\\(extends *[a-zA-Z]*\\)? *\\(implements [a-zA-Z]*\\( *, *[a-zA-Z]\\)?\\)?")
                (move-beginning-of-line 1)
                (newline)
                (insert (format "@inject(%s)\n" import-name))))
            (progn
              (search-forward-regexp "\\(export\\)? *class [A-Za-z] *\\(extends *[a-zA-Z]*\\)? *\\(implements [a-zA-Z]*\\( *, *[a-zA-Z]\\)?\\)?")
              (move-end-of-line 1)
              (newline)
              (insert (format "private %s: %s" import-name var-name)))

            (if (ignore-errors (search-forward-regexp "constructor("))
                (progn
                  (forward-char)
                  (insert (format "%s: %s, " var-name import-name)))
              (progn
                (move-end-of-line 1)
                (newline)
                (insert (format "constructor(%s: %s) {\n\n}" var-name import-name))))))))))



(defun buffer-contains-class ()
  "throws 'aurelia-inject-exit error if no class is found"
  (save-excursion
    (save-match-datap
      (goto-char (point-min))
      (condition-case nil
          (search-forward-regexp "\\(export\\)? *class [A-Za-z] *\\(extends *[a-zA-Z]*\\)? *\\(implements [a-zA-Z]*\\( *, *[a-zA-Z]\\)?\\)?")
        (error (message "Buffer must contain a class to inject into.")
               (throw 'aurelia-inject-exit nil))))))
