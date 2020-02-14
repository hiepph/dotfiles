(require 'init-elpa)
(require 's)


;; ACME
;; REF: http://man.cat-v.org/plan_9/1/acme
;;
(defun ~acmec (&optional command)
  (interactive)
  (let (($buf (generate-new-buffer "*+Errors*")))
    (with-current-buffer $buf
      (goto-char (point-max))
      (insert (shell-command-to-string command)))
    (display-buffer $buf))
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


(defun ~compile ()
  "Compile the current file. A replacement for compile with automatic filetype recognition.
e.g. If the current buffer is hello.py, then it'll call python hello.py
"
  (interactive)
  (let (($suffix-map '(("py" . "python")
                       ("go" . "go run")))
        $fname
        $suffix
        $prog)
    (setq $fname (buffer-file-name))
    (setq $suffix (file-name-extension $fname))
    (setq $prog (cdr (assoc $suffix $suffix-map)))
    (setq $command (concat $prog " \"" $fname "\""))
    (message $command)
    (~acmec $command)
    ))


(provide 'init-func)
