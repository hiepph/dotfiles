(require 'init-elpa)
(require 's)


;; ACME
;; REF: http://man.cat-v.org/plan_9/1/acme
;;
(defun ~acme! (&optional command)
  "Execute command and output to *+Errors** buffer

sample:
! ls
"
  (interactive)
  (let (($buf (generate-new-buffer "*+Errors*")))
    (with-current-buffer $buf
      (goto-char (point-max))
      (insert (shell-command-to-string command)))
    (display-buffer $buf))
  )


(defun ~acme$ (&optional command)
  "spawn a terminal and execute command

sample:
$ ls
"
  (interactive "M$: ")
  (setq term "urxvt -e $SHELL -c")
  (setq toggle-floating "i3-msg floating enable > /dev/null")
  (call-process-shell-command (format "%s '%s; %s; $SHELL -i'" term command toggle-floating)
                 nil)
  )


(defun ~acme< (&optional command)
  (interactive)
  (delete-region (region-beginning)
                 (region-end))
  (insert (shell-command-to-string command))
  )


;; (defun ~acme| (&optional command)
;;   (interactive "MCommand: ")
;;   (let ((inhibit-message t))
;;     (shell-command-on-region (region-beginning)
;;                              (region-end)
;;                              command
;;                              ;; output
;;                              (current-buffer)
;;                              ;; replace?
;;                              t
;;                              ;; name of error buffer (nil = current-buffer)
;;                              nil
;;                              ;; show error buffer
;;                              0)))


;;
;; Buffers
;;
(defun ~kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


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
                               ))

  (~run-current-file *test-command-map*)
  )




(provide 'init-func)
