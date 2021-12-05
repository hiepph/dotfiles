;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; Auto-indentation
(electric-indent-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enter automatically indent code
(define-key global-map (kbd "RET") 'newline-and-indent)


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
;; ref: https://github.com/magnars/expand-region.el
;;
(use-package expand-region)


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit)


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
;; Direnv integration:
;; ref: https://github.com/wbolster/emacs-direnv
;;
(use-package direnv)


;;
;; Jump to definition, even without CTAGS
;; ref: https://github.com/jacktasia/dumb-jump
;;
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;
;; Compile
;;

(defun ~compile (command)
  (interactive "M~compile: ")
  (compile (s-replace "%" (evil-get-register ?% t) command)))

(provide 'core-editor)
