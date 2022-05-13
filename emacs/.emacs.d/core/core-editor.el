(require 'ansi-color)


;; Highlight current line
(global-hl-line-mode 1)


;;
;; Tabs
;;

;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; View tabs as 4 spaces
(setq default-tab-width 4)

;; tab stop position
(setq tab-stop-list '(4 8 12))


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
;; Indentation
;;

;; Auto-indentation
(electric-indent-mode 1)

;; Enter automatically indent code
(define-key global-map (kbd "RET") 'newline-and-indent)

;;
;; Automatic indentation offset detection
;;
(use-package dtrt-indent
  :config
  (dtrt-indent-global-mode))


;;
;; Pairs
;;
;; Highlights matching parenthesis
(show-paren-mode 1)

;; highlight brackets if visible, else entire expression
(setq show-paren-style 'mixed)

;; autopair
(electric-pair-mode)

;;
;; paredit, supports barfing and slurping
;;
(use-package paredit
  :hook
  ((emacs-lisp-mode
    ielm-mode
    clojure-mode
    cider-mode
    cider-repl-mode
    racket-mode
    scheme-mode) . paredit-mode))


;;
;; Expand region
;; ref: https://github.com/magnars/expand-region.el
;;
(use-package expand-region)


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit)


;;
;; diff-hl: highlight git commit changes
;; ref: https://github.com/dgutov/diff-hl
;;
(use-package diff-hl
  :after (magit)
  :config
  (global-diff-hl-mode)

  ;; auto-refresh with magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; turn on when open dired
  (add-hook 'dired-mode-hook
          (lambda ()
            (diff-hl-dired-mode 1))))


;;
;; Whitespace
;; ref: https://www.emacswiki.org/emacs/WhiteSpace
;;
(use-package whitespace
  :config
  (global-whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; `lines-tail` highlight part of lines that goes beyond ‘whitespace-line-column’
  ;; `trailing` highlight trailing white spaces
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face trailing tab)))


;;
;; Column indicator
;;
(use-package fill-column-indicator
  :init
  (setq fci-rule-column 80))


;;
;; Flycheck
;;
(use-package flycheck)


;;
;; Flyspell
;;
;; disable for log edit and change log
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

;;
;; Company
;;
(use-package company
  :config
  ;; complete word with original cases
  (setq company-dabbrev-downcase nil))


;;
;; Undo/Redo
;; ref: https://gitlab.com/ideasman42/emacs-undo-fu
;;
(use-package undo-fu)


;;
;; Direnv integration.
;; Operate buffer-locally compared to direnv.el
;; ref: https://github.com/purcell/envrc
;;
(use-package inheritenv
  :straight (inheritenv
             :type git
             :host github
             :repo "purcell/inheritenv"
             :branch "main"))

(use-package envrc
  :straight (envrc
             :type git
             :host github
             :repo "purcell/envrc")
  :init
  (envrc-global-mode))

;;
;; Jump to definition, even without CTAGS
;; ref: https://github.com/jacktasia/dumb-jump
;;
(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)

  (setq dumb-jump-selector 'completing-read)

  ;; dump-jump-back is useful but is cognitively overhead
  ;; mark position before jump to definition so we can go back with C-o
  (advice-add 'dumb-jump-go :before (lambda (&rest r) (evil-set-jump))))


;;
;; Auto scroll with compilation output.
;;
(setq compilation-scroll-output t)

;; Let it roll to the first error.
;; (setq compilation-scroll-output 'first-error)


;;
;; Custom Compile that supports special symbols
;;
;; +) %: current filename
;;
(defun ~compile (command)
  (interactive "M~compile: ")
  (save-buffer)
  (compile (s-replace "%" (evil-get-register ?% t) command)))


;;
;; Mod recompile
;; + Auto save-buffer
;;
(defun ~recompile ()
  (interactive)
  (save-buffer)
  (recompile))

;;
;; Enable ANSI coloring for compilation buffer
;;
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;
;; Quickly open terminal in current directory.
;; (supports only Mac for now)
;;
(defun ~open-terminal ()
  (interactive)
  (shell-command "open . -a iTerm"))

;;
;; Quickly open file manager in current directory.
;; (supports only Mac for now)
;;
(defun ~open-file-manager ()
  (interactive)
  (shell-command "open ."))

;;
;; ACME modifies text with shell-command
;;
(defun ~acme> (command)
  "run shell command and output out result to current position"
  (interactive "M>: ")
  (insert (shell-command-to-string command)))

(defun ~acme| (command)
  "run shell command on region and replace it"
  (interactive "M|: ")
  (shell-command-on-region (region-beginning)
                           (region-end)
                           command
                           ;; output
                           (current-buffer)
                           ;; replace?
                           t))

(defun ~acme$ (command)
  "Open terminal and run shell command.
Supports MacOS (with Hammerspoon) for now."
  (interactive "M$: ")
  (shell-command
   (format "open -g 'hammerspoon://terminal?dir=%s&command=%s'" default-directory command)))

(provide 'core-editor)
