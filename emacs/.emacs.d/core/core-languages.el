;; Lisp
;; (use-package slime)
;; (setq inferior-lisp-program (executable-find "sbcl"))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")



;; Clojure
(use-package cider)

;; Racket
(use-package racket-mode)


;; markdown
(use-package markdown-mode)

;; ugly code font problem
;; (set-face-attribute 'markdown-code-face nil :font "Hack")

;; Python
;;
;; using autopep8 for auto-formatting python
;; refer: https://github.com/hhatto/autopep8
;;
(defun ~format-python ()
  "Format python code followed PEP8"
  (interactive)
  (when (eq major-mode 'python-mode)
    (let ((fname (buffer-file-name)))
      (shell-command (format "autopep8 --in-place --aggressive %s" fname) nil))))

;; C
;;
;; autoformat C code using GNU's `indent`
;; refer: https://www.gnu.org/software/indent/manual/indent.html
;;
(defun ~format-c ()
  "Format C code followed GNU style."
  (interactive)
  (when (eq major-mode 'c-mode)
    (let ((fname (buffer-file-name)))
      ;; some modification
      ;; -bad: leave a blank line after definitions
      ;; -br: if (...) {
      ;; -ce: } else {
      (shell-command (format "indent -bad -br -npcs -ce %s" fname) nil))))

;; Go
(use-package go-mode
  :config
  ;; format and import before saving
  (setq gofmt-command "goimports") ; requires "goimports"
  (add-hook 'before-save-hook 'gofmt-before-save))


;; Haskell
(use-package haskell-mode)

(defun haskell-evil-open-above ()
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")


;; Yaml
(use-package yaml-mode)

;; Org-mode
(use-package org
  :init
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  ;; show image in org babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Turn off auto-fold
  (setq org-startup-folded nil)

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)
  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  :config
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (emacs-lisp . t)
     (python     . t)
     (ruby       . t))
   ))

(require 'org-tempo)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-async)


;;
;; Custom
;;
(add-to-list 'auto-mode-alist '("\\.scheme\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))


;;
;; Auto format mode
;;
(define-minor-mode format-mode
  "Auto format code using predefined formatter"
  :lighter "format"
  :global t)

;; enable: add hook auto-format
;; disable: remove hook auto-format
(add-hook 'format-mode-hook
          (lambda ()
            (if format-mode
                (progn
                  (add-hook 'after-save-hook '~format-python)
                  (add-hook 'after-save-hook '~format-c))
              (progn
                (remove-hook 'after-save-hook '~format-python)
                (remove-hook 'after-save-hook '~format-c)))))


;; View tabs as 4 spaces
(setq default-tab-width 4)
(setq tab-width 4)


(provide 'core-languages)
