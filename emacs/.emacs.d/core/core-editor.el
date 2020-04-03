;;
;; Basic
;;

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; use bash as default shell
;; (setq-default explicit-shell-file-name "/bin/bash")

;; open *shell* in current buffer
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; auto revert dired-mode
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; replace highlight text with typing action
(delete-selection-mode 1)

;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; View tabs as 4 spaces
(setq default-tab-width 4)

;; Tab as 4 spaces
(setq tab-width 4)

;; Auto-indentation
(electric-indent-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enter automatically indent code
(define-key global-map (kbd "RET") 'newline-and-indent)


;;
;; Automatic indentation offset detection
;;
(use-package dtrt-indent
  :init
  (dtrt-indent-mode)
  )


;;
;; Pairs
;;
;; Highlights matching parenthesis
(show-paren-mode 1)


;; Autopair
(use-package autopair
  :init
  (autopair-global-mode))

;; Expand region
(use-package expand-region)

;; Rainbow parentheses
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Surround
;; ref: https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

;; Search for selected region
(use-package evil-visualstar
  :init
  (global-evil-visualstar-mode))


;;
;; Whitespace
;;
(use-package whitespace
  :config
  (global-whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq whitespace-line-column 100)
  ;; `lines-tail` highlight part of lines that goes beyond ‘whitespace-line-column’ (default: 80)
  ;; `trailing` highlight trailing white-spaces
  (setq whitespace-style '(face lines-tail trailing))
)


;;
;; Commenter
;; ref: https://github.com/linktohack/evil-commentary
;;
(use-package evil-commentary
  :init
  (evil-commentary-mode)
  )


;;
;; Undo/Redo tree
;;
(use-package undo-tree
  :init
  (global-undo-tree-mode))



;;
;; Build and test
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


(defvar *compilation-command-map* '(("py" . "python")
                                    ("go" . "go run")
                                    ("rb" . "ruby")
                                    ("rb" . "ruby")
                                    ("hs" . "runhaskell")
                                    ("sh" . "bash")
                                    ))
(defun ~compile-current-file ()
  "(re)compile the current file. A replacement for compile with automatic filetype recognition.
e.g. If the current buffer is hello.py, then it'll call python hello.py
"
  (interactive)
  (~run-current-file *compilation-command-map*))


(defvar *test-command-map* '(("py" . "pytest")
                            ("go" . "go test")))
(defun ~test-current-file ()
  "Test current file using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest hello.py
"
  (interactive)
  (~run-current-file *test-command-map*))


(defun ~test-all-files ()
  "Test all files in same directory using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest
"
  (interactive)
  (save-buffer)

  (let* ((fname (buffer-file-name))
         (suffix (file-name-extension fname))
         (prog (cdr (assoc suffix *test-command-map*)))
         (command prog))
    (if (null prog)
        (error "Compile command not found. Please check '*<?>-command-map*'")
      (compile command))))


;;
;; ACME
;; REF: http://man.cat-v.org/plan_9/1/acme
;;
(use-package s)

(defvar record-separator "%")

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
      (insert (format "! %s\n%s%s\n" trimmed-command (shell-command-to-string cmd) record-separator)))

    ;; switch to *+Errors+* buffer
    (switch-to-buffer-other-window buf)
    (goto-address-mode t)
    (goto-char (point-max))))



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
;; Execute text by pattern
;; ref: https://github.com/cmpitg/wand
;;
;; (use-package wand
;;   :config
;;   (setq wand:*rules*
;;         (list
;;          (wand:create-rule :match (rx bol (0+ " ") "!")
;;                            :capture :after
;;                            :action #'~acme!)
;;          (wand:create-rule :match (rx bol (0+ " ") "$")
;;                            :capture :after
;;                            :action #'~acme$)
;;          (wand:create-rule :match (rx bol (0+ " ") "<")
;;                            :capture :after
;;                            :action #'~acme<)
;;          (wand:create-rule :match (rx bol (0+ " ") "http")
;;                            :capture :whole
;;                            :action #'browse-url-firefox)
;;          )))



;;
;; Buffers
;;
(defun ~eval-buffer ()
  "eval-buffer but with message"
  (interactive)
  (eval-buffer)
  (message "> Eval buffer succeeded"))



;;
;; Multiple cursors
;;
(use-package evil-mc
  :init
  (global-evil-mc-mode))


(provide 'core-editor)
