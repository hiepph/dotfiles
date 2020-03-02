(require 'init-elpa)
(require 's)


;; ACME
;; REF: http://man.cat-v.org/plan_9/1/acme
;;

(defun ~acme! (&optional command)
  "Execute command using (async) shell command

sample:
! ls
"
  (interactive "M!: ")
  (let* ((buf (get-buffer-create "*+Errors*"))
        (trimmed-command (s-trim command))
        (cmd (format "%s &" trimmed-command)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "! %s\n%s\n" trimmed-command (shell-command-to-string cmd))))
    (display-buffer buf)))



(defun ~acme$ (&optional command)
  "spawn a terminal and execute command

sample:
$ ls
"
  (interactive "M$: ")
  (setq open-term "urxvt -e $SHELL -c")
  (setq toggle-floating "i3-msg floating enable > /dev/null")
  (call-process-shell-command (format "%s '%s; %s; $SHELL -i'" open-term toggle-floating command)
                 nil)
  )


(defun ~acme< (&optional command)
  (interactive "M<: ")
  (delete-region (region-beginning)
                 (region-end))
  (insert (s-trim-right (shell-command-to-string command)))
  )


(defun ~acme| (&optional command)
  (interactive "M|: ")
  (let ((inhibit-message t))
    (shell-command-on-region (region-beginning)
                             (region-end)
                             command
                             ;; output
                             (current-buffer)
                             ;; replace?
                             t
                             ;; name of error buffer (nil = current-buffer)
                             nil
                             ;; show error buffer
                             0)))


;;
;; Buffers
;;
(defun ~kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun ~open-shell ()
  "Open shell in current buffer, and follows current directory.
If *shell* buffers is already in another buffer, kill it and starts new."
  (interactive)
  (kill-buffer "*shell*")
  (shell)
  )



;;
;; Programming
;;
(defun ~run-current-file (command-map)
  (interactive)
  (save-buffer)

  (let* ((fname (buffer-file-name))
         (suffix (file-name-extension fname))
         (prog (cdr (assoc suffix command-map)))
         (command (format "%s %s" prog (shell-quote-argument fname))))
    (if (null prog)
        (error "Compile command not found. Please check '*<?>-command-map*'")
      (compile command))))


(defun ~compile-current-file ()
  "(re)compile the current file. A replacement for compile with automatic filetype recognition.
e.g. If the current buffer is hello.py, then it'll call python hello.py
"
  (interactive)
  (defvar *compilation-command-map* '(("py" . "python")
                                      ("go" . "go run")
                                      ("hs" . "runhaskell")))
  (~run-current-file *compilation-command-map*)
  )


(defun ~test-current-file ()
  "Test current file using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest hello.py
"
  (interactive)
  (defvar *test-command-map* '(("py" . "pytest")
                               ("go" . "go test")
                               ))

  (~run-current-file *test-command-map*)
  )


(provide 'init-func)
