;;; $DOOMDIR/helper.el -*- lexical-binding: t; -*-

(defun ~consult-shell-history (query)
  "Search and insert shell's history command"
  (interactive "MShell history: ")
  (let ((history (s-split
                  "\n"
                  (shell-command-to-string
                   (if (string-empty-p query)
                       "history"
                     (format "history search --contains '%s'" query))))))
    (insert (completing-read "history: "
                             (seq-filter (lambda (str) (not (s-blank? str))) history)))))

(defun ~shell-command-insert (command)
  (interactive "MCommand: ")
  (insert (shell-command-to-string command)))
