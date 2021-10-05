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
;; (setq-default explicit-shell-file-name "/bin/bash")
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
;;
(use-package expand-region)


;;
;; Evil
;;


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit)


;; Evil keybinding
;; ref: https://github.com/emacs-evil/evil
(use-package evil
  :after magit
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)

  :config
  (evil-mode 1))

;; Evil collection - pre-keybindings for common Emacs modes
;; pre: https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-magit-state 'normal)
  :custom
  ;; enable Evil in minibuffer
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Evil keybindings for Magit
;; Notes:
;;   + this is included inside evil-collection, but somehow it doesn't work,
;;     needs further investigation
(use-package evil-magit
  :after (evil magit))

;; Surround
;; ref: https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Search for selected region (especially word)
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; Evil fringe mark - show markers (e.g. `m m')
;; ref: https://github.com/Andrew-William-Smith/evil-fringe-mark
(use-package evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode 1))

;;
;; Commenter
;; ref: https://github.com/linktohack/evil-commentary
;;
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Multiple cursors
;; ref: https://github.com/gabesoft/evil-mc
;; keybinding: https://github.com/gabesoft/evil-mc/blob/master/evil-mc.el
;;
;; * Note:
;; evil-mc and visual selection mode can play nicely together
;; Suppose I selected some text in visual mode, then g-r-I to active multiple cursors
(use-package evil-mc
  :config
  (global-evil-mc-mode))



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
  (setq fci-rule-column 100))


;;
;; Flycheck
;;
(use-package flycheck
  :config
  (global-flycheck-mode))


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
  :config
  (fset 'multi-occur #'consult-multi-occur)

  ;; :config
  ;; (consult-preview-mode)
  )

(use-package consult-flycheck)


;;
;; Marginalia
;;
(use-package marginalia
  :config
  (marginalia-mode)
  (advice-add
   #'marginalia-cycle :after
   (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))


;;
;; Direnv integration:
;; ref: https://github.com/wbolster/emacs-direnv
;;
(use-package direnv)


(provide 'core-editor)
