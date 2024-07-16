;;; $DOOMDIR/helper.el -*- lexical-binding: t; -*-

(defun ~shell-command-insert (command)
  (interactive "MCommand: ")
  (insert (shell-command-to-string command)))
