(defun switch-to-minibuffer()
       "Switch to minibuffer window."
       (interactive)
       (if (active-minibuffer-window)
	   (select-window(active-minibuffer-window))
	 (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer)

(defun replace-in-subfolders ()
  (interactive)
    (let ((query-replace (read-string "Query replace:"))
          (query-replace-with (read-string "Query replace with:"))
          (query-replace-path (read-string "Path:")))

      (shell-command (format "find %s -type f -exec sed -i '' s/%s/%s/g {} \\;" query-replace-path query-replace query-replace-with))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun copy-line()
       "Copies a line and pastes it to the next line"
       (interactive)
       (move-beginning-of-line 1)
       (kill-line)
       (yank)
       (newline)
       (yank))

(defun dirgrep ()
  "Run grep recursively from the directory of the current buffer or the default directory"
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep -nH -r  " dir) 13))))
      (grep command))))





;;---------------ES6 import sort-----------------------

;; (defun next-import ()
;;   "Goes to beginning of next import"
;;   (interactive)
;;   (whereami)
;;   (re-search-forward "^import.*from.*$")
;;   (move-beginning-of-line 1))

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
  (quote nil)
  )

(defun import-sort ()
    "Typescript/ES6 import sort"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (next-import)
          (sort-subr nil 'next-import 'end-of-line 'import-start-key 'import-start-key)))

(defadvice dired-mark-read-file-name (after rv:dired-create-dir-when-needed (prompt dir op-symbol arg files &optional default) activate)
  (when (member op-symbol '(copy move))
    (let ((directory-name (if (< 1 (length files))
                              ad-return-value
                              (file-name-directory ad-return-value))))
      (when (and (not (file-directory-p directory-name))
                 (y-or-n-p (format "directory %s doesn't exist, create it?" directory-name)))
        (make-directory directory-name t)))))
