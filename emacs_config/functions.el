(defun switch-to-minibuffer()
       "Switch to minibuffer window."
       (interactive)
       (if (active-minibuffer-window)
	   (select-window(active-minibuffer-window))
	 (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer)



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

