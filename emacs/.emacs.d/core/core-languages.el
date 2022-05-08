;;
;; LSP mode
;; ref: https://github.com/emacs-lsp/lsp-mode
;;
(use-package lsp-mode
  :hook ((java-mode) . lsp-deferred)
  :commands lsp)

;;
;; Java
;; ref: https://github.com/emacs-lsp/lsp-java
;;
(use-package dash-functional)

(use-package lsp-java
  :requires (dash-functional)
  :after lsp)

;;
;; Clojure
;;
(use-package cider
  :config
  ;; disable warning when jacking-in outside a project
  (setq cider-allow-jack-in-without-project t)

  ;; Make cider use ~boot~ by default
  (setq cider-jack-in-default 'boot))

(defun ~format-clojure ()
  (interactive)
  (let ((fname (buffer-file-name)))
    (shell-command (format "zprint -w %s" fname) nil)))

;;
;; Racket
;;
(use-package racket-mode)


;;
;; markdown
;;
(use-package markdown-mode)


;;
;; julia
;;
(use-package julia-mode)


;;
;; Python
;;
;; using 'black' for auto-formatting python
;; ref: https://github.com/psf/black
(defun ~format-python ()
  "Format python code followed PEP8"
  (interactive)
  (let ((fname (buffer-file-name)))
    (shell-command (format "black %s --skip-string-normalization" fname) nil)))

;;
;; Ruby
;;
(defun ~format-ruby ()
  "Format Ruby code followed Ruby style guide"
  (interactive)
  (let ((fname (buffer-file-name)))
    (shell-command (format "rubocop --auto-correct --format quiet --stderr %s" fname) nil)))


;;
;; Zig
;;
(use-package zig-mode)


:;
;; C
;;

;; autoformat C code using GNU's `indent`
;; refer: https://www.gnu.org/software/indent/manual/indent.html
(defun ~format-c ()
  "Format C code followed GNU style."
  (interactive)
  (let ((fname (buffer-file-name)))
    ;; some modification
    ;; -bad: leave a blank line after definitions
    ;; -br: if (...) {
    ;; -ce: } else {
    (shell-command (format "indent -bad -br -npcs -ce %s && rm %s~" fname fname) nil)))


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


;;
;; Nix
;;
(use-package nix-mode
  :mode "\\.nix\\'")


;;
;; Fish
;;
(use-package fish-mode)

;;
;; Lua
;;
(use-package lua-mode)


;;
;; Yaml
;;
(use-package yaml-mode)


;;
;; Terraform
;;
(use-package terraform-mode)


;;
;; SQL
;;
(use-package sql-indent)


;;
;; Docker
;;
(use-package dockerfile-mode)

;;
;; Org-mode
;;
(use-package org
  :init
  (setq org-log-done 'time)

  ;; hide emphasis (WYSIWYG)
  (setq org-hide-emphasis-markers t)

  ;; disable sub/superscript when using '^' or '_'
  ;; equivalence in org file: `#+OPTIONS: ^:nil`
  (setq org-export-with-sub-superscripts nil)

  ;; show image in org babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Turn off auto-fold
  (setq org-startup-folded nil)

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)

  ;; For languages with significant whitespace like Python
  (setq org-src-preserve-indentation t)

  ;; Turn on image by default
  (setq org-startup-with-inline-images t)

  ;; Don't display full width of the image
  (setq org-image-actual-width nil)

  ;; Turn on latex view by default
  (setq org-startup-with-latex-preview t)

  ;; show special symbols
  (setq org-pretty-entities t)

  ;; auto-indent
  (setq org-startup-indented t)

  ;; preserve indentation when export
  (setq TeX-auto-untabify t)

  ;; order list with a. b. c.
  (setq org-list-allow-alphabetical t)

  :config
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     (lua . t)
     (latex . t)
     (dot . t)))

  :hook
  ;; remove '<' autopair
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<)
                                   t
                                 (,electric-pair-inhibit-predicate c))))))
  (org-mode . (lambda ()
                (setq-local evil-shift-width 2))))

;; Expand snippets defined in ~org-structure-template-alist~
;; e.g. <s TAB expands to ~#+begin_src~
(require 'org-tempo)

;; Enable async for code block
(use-package ob-async)

;; Beautify heading lists
;; ref: https://github.com/sabof/org-bullets
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))


;;
;; DOT-mode for Graphviz
;;
(use-package graphviz-dot-mode)


;;
;; Recognize language mode for some extensions
;;
(add-to-list 'auto-mode-alist '("\\.scheme\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.Vagrantfile\\'" . ruby-mode))


;;
;; format-mode
;; Automatically format code based on language mode
;;
;; ref:
;; + https://github.com/lassik/emacs-format-all-the-code
;;
(defgroup format nil
  "Automatically format code based on language mode"
  :group 'format)

;; Define your format code here
(defcustom format-formatters
  '((python-mode ~format-python)
    (c-mode ~format-c)
    (ruby-mode ~format-ruby)
    (clojure-mode ~format-clojure))
  "Default formatters for predefined languages"
  :type '(repeat (list symbol symbol))
  :group 'format)

(defun ~format-code ()
  "Format code based on language mode"
  (interactive)
  (let ((format-func (assoc major-mode format-formatters)))
    (when format-func
        (progn (funcall-interactively (car (cdr format-func)))
               (revert-buffer t t t)))))

(define-minor-mode format-mode
  "Auto format code using predefined formatter"
  :lighter "format"
  :global nil
  (if format-mode
      (add-hook 'after-save-hook
                '~format-code
                nil 'local)
    (remove-hook 'after-save-hook
                 '~format-code
                 'local)))

(define-globalized-minor-mode format-global-mode
  format-mode
  (lambda ()
    (format-mode t)))


;;
;; Yasnippet
;; ref: https://github.com/joaotavora/yasnippet
;;
(use-package yasnippet
  :config
  (yas-global-mode 1))


(provide 'core-languages)
