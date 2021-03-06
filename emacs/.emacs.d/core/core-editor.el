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
;; put all backups in ~/.emacs.d/backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; open *shell* in current buffer
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; replace highlight text with typing action
(delete-selection-mode 1)

;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; Auto-indentation
(electric-indent-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enter automatically indent code
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)


;;
;; Tramp
;;
(setq-default explicit-shell-file-name "/bin/bash")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)


;;
;; Make sure Emacs uses $PATH to execute command in shell
;; (e.g. MacOS)
;; ref: https://github.com/purcell/exec-path-from-shell
;;
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;
;; Automatic indentation offset detection
;;
(use-package dtrt-indent
  :init
  (dtrt-indent-global-mode))


;;
;; Pairs
;;
;; Highlights matching parenthesis
(show-paren-mode 1)
;; highlight brackets
;; (setq show-paren-style 'parenthesis)
;; highlight entire expression
;; (setq show-paren-style 'expression)
;; highlight brackets if visible, else entire expression
(setq show-paren-style 'mixed)

;; autopair
(electric-pair-mode)

;;
;; paredit, supports barfing and slurping
;;
(use-package paredit
  :diminish
  :config
  :hook
  ((emacs-lisp-mode
    ielm-mode
    clojure-mode
    cider-mode
    cider-repl-mode
    racket-mode
    scheme-mode) . paredit-mode))


;; Expand region
(use-package expand-region)


;; Rainbow parentheses
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;
;; Evil keybinding
;; ref: https://github.com/emacs-evil/evil
;;
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)

  :config
  (evil-mode 1))


;;
;; Evil escape
;; quickly escape to normal state using 'fd'
;; ref: https://github.com/syl20bnr/evil-escape
;;
(use-package evil-escape
  :after evil
  :diminish
  :init
  (evil-escape-mode))

(use-package evil-magit
  :after evil
  :diminish)


;;
;; Disable mouse
;; Reduce the annoying event of accidentally touching mouse that activates visual selection mode
;; ref: https://github.com/purcell/disable-mouse
;;
(use-package disable-mouse
  :after evil
  :init
  (global-disable-mouse-mode)
  :config
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

;;
;; Evil collection
;;
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'compile))

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
;; Evil-snipe
;;
(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (setq evil-snipe-scope 'buffer))


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
  (setq whitespace-style '(face lines-tail trailing)))
(add-hook 'text-mode-hook 'auto-fill-mode)

;;
;; Commenter
;; ref: https://github.com/linktohack/evil-commentary
;;
(use-package evil-commentary
  :init
  (evil-commentary-mode))


;;
;; Compile
;;

;; show stack trace on error
;; (setq debug-on-error t)
(defun ~compile (command)
  (interactive "M~compile: ")
  (compile (s-replace "%" (evil-get-register ?% t) command)))

(defun ~run-current-file (f command-map)
  "Run command map with function f
f can be: compile, ~acme$, ~acme&, ~acme!"
  (interactive)
  (save-buffer)

  (let* ((fname (s-chop-suffix (car (s-match "<.*>" (buffer-name))) (buffer-name)))
         (suffix (file-name-extension fname))
         (prog (cdr (assoc suffix command-map))))
    (if (null prog)
        (error "Extension is not yet registered")
      (funcall f (format "%s %s" prog (shell-quote-argument fname))))))

(defvar *compile-command-map* '(("py" . "python")
                                ("go" . "go run")
                                ("rb" . "ruby")
                                ("hs" . "runhaskell")
                                ("sh" . "bash")))

(defun ~compile-current-file ()
  "(re)compile the current file. A replacement for compile with automatic filetype recognition.
e.g. If the current buffer is hello.py, then it'll call python hello.py
"
  (interactive)
  (save-buffer)
  (~run-current-file 'compile *compile-command-map*))

;; default compile command to empty
(setq compile-command "")

(defun ~recompile ()
  "custom recompile "
  (interactive)
  (save-buffer)
  (recompile))

(defvar *test-command-map* '(("py" . "pytest -s -v")
                             ("go" . "go test")))

(defun ~test-current-file ()
  "Test current file using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest hello.py
"
  (interactive)
  (~run-current-file 'compile *test-command-map*))

(defun ~test-all-files ()
  "Test all files in same directory using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest
"
  (interactive)
  (save-buffer)

  (let* ((fname (buffer-name))
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

;; TODO
(defun ~acme$ (&optional command)
  "spawn a terminal and execute command

eg:
$ ls
"
  (interactive "M$: ")
  (let ((open-term "alacritty -e $SHELL -c"))
    (call-process-shell-command
     (format "%s '%s; $SHELL -i'" open-term command)
     nil)))


(defun ~acme< (&optional command)
  "Execute command and pipe stdout to editor

eg:
< ls
"
  (interactive "M<: ")
  (delete-region (region-beginning)
                 (region-end))
  (insert (s-trim-right (shell-command-to-string command))))


(defun ~acme| (&optional command)
  "Pipe input into command line and output stdout to editor"
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
;; WARNING: wand-helper:maybe-uncomment-string
;; ref: https://github.com/cmpitg/wand
;; < ls
;;
;; (use-package wand
;;   :config
;;   (setq wand:*rules*
;;         (list
;;          (wand:create-rule :match (rx bol (0+ " ") "&")
;;                            :capture :after
;;                            :action #'async-shell-command)
;;          (wand:create-rule :match (rx bol (0+ " ") "$")
;;                            :capture :after
;;                            :action #'~acme$)
;;          (wand:create-rule :match (rx bol (0+ " ") "<")
;;                            :capture :after
;;                            :action #'~acme<)
;;          (wand:create-rule :match "https?://"
;;                               :capture :whole
;;                               :action #'browse-url-firefox))))


;;
;; Buffers
;;
(defun ~eval-buffer ()
  "eval-buffer but with message"
  (interactive)
  (eval-buffer)
  (message "> Eval buffer succeeded"))


(defun ~kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ~kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;;
;; Multiple cursors
;; ref: https://github.com/gabesoft/evil-mc
;; keybinding: https://github.com/gabesoft/evil-mc/blob/master/evil-mc.el
;;
;; * Note:
;; evil-mc and visual selection mode can play nicely together
;; Suppose I selected some text in visual mode, then g-r-I to active multiple cursors
;;
(use-package evil-mc
  :init
  (global-evil-mc-mode))


;;
;; Display mark position in fringe
;;
(use-package evil-fringe-mark
  :init
  (global-evil-fringe-mark-mode)
  ;; show special marks
  ;; (setq-default evil-fringe-mark-show-special t)
  )


;;
;; Flycheck
;;
(use-package flycheck
  :init (global-flycheck-mode))


;;
;; Flyspell
;;

;; enable for some mode
(dolist (hook '(markdown-mode-hook git-commit-mode))
    (add-hook hook (lambda () (flyspell-mode 1))))
;; disable for log edit and change log
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))


;;
;; Company
;;
(use-package company
  :config
  (global-company-mode)
  ;; complete word with original cases
  (setq company-dabbrev-downcase nil))


;;
;; Undo/Redo
;; ref: https://gitlab.com/ideasman42/emacs-undo-fu
;;
(use-package undo-fu)


;;
;; consult
;; ref: https://github.com/minad/consult
;;
(use-package consult
  :init
  (fset 'multi-occur #'consult-multi-occur)
  ;; :config
  ;; (consult-preview-mode)
  )

(use-package consult-flycheck)

;;
;; Marginaalia
;;
(use-package marginalia
  :init
  (marginalia-mode)
  (advice-add
   #'marginalia-cycle :after
   (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))


(provide 'core-editor)
