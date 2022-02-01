;; Lisp
;; (use-package slime)
;; (setq inferior-lisp-program (executable-find "sbcl"))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")


;; Clojure
(use-package cider
  :config
  ;; disable warning when jacking-in outside a project
  (setq cider-allow-jack-in-without-project t)

  ;; Make cider use ~boot~ by default
  (setq cider-jack-in-default 'boot))

;; Racket
(use-package racket-mode)

;; markdown
(use-package markdown-mode)

;; julia
(use-package julia-mode)


;;
;; Python
;;

;; using 'black' for auto-formatting python
;; ref: https://github.com/psf/black
;;
(defun ~format-python ()
  "Format python code followed PEP8"
  (interactive)
  (when (eq major-mode 'python-mode)
    (let ((fname (buffer-file-name)))
      (shell-command (format "black %s" fname) nil))))

:;
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
      (shell-command (format "indent -bad -br -npcs -ce %s && rm %s~" fname fname) nil))))

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


;; Lua
(use-package lua-mode)


;; Yaml
(use-package yaml-mode)

;;
;; Org-mode
;;
(use-package org
  :init
  (setq org-log-done 'time)

  ;; hide emphasis (WYSIWYG)
  (setq org-hide-emphasis-markers t)

  ;; show items in list with circular bullet
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-+*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; show image in org babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Turn off auto-fold
  (setq org-startup-folded nil)

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)

  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  ;; Turn on image by default
  (setq org-startup-with-inline-images t)

  ;; Don't display full width of the image
  (setq org-image-actual-width nil)

  :config
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (emacs-lisp . t)
     (python     . t)
     (ruby       . t)))

  :hook
  ;; remove '<' autopair
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<)
                                   t
                                 (,electric-pair-inhibit-predicate c)))))))

;;
;; Expand snippets defined in ~org-structure-template-alist~
;; e.g. <s TAB expands to ~#+begin_src~
;;
(require 'org-tempo)

(use-package ob-async)

;; Beautify heading lists
;; ref: https://github.com/sabof/org-bullets
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

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
