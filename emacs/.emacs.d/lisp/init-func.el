(require 'init-elpa)


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
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init-func)
