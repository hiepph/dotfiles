(require 'init-elpa)

;; Basic
;; replace highlight text with typing action
(delete-selection-mode 1)

;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; View tabs as 4 spaces
(setq default-tab-width 4)

;; Tab as 4 spaces
(setq tab-width 4)

;; Autopair
(use-package autopair
  :ensure t
  :diminish autopair-mode

  :init
  ;; auto-wrap word into pair
  (setq autopair-autowrap t)

  :config
  (autopair-global-mode)
  ;; Triple quote in python
  (add-hook 'python-mode-hook
            #'(lambda ()
                (setq autopair-handle-action-fns
                      (list #'autopair-default-handle-action
                            #'autopair-python-triple-quote-action)))))


;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))


;; Trailing white-space
(require 'whitespace)

(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq whitespace-line-column 100)
;; `lines-tail` highlight part of lines that goes beyond ‘whitespace-line-column’ (default: 80)
;; `trailing` highlight trailing white-spaces
(setq whitespace-style '(face lines-tail trailing))

;; Indent
;; Auto-indentation
(electric-indent-mode 1)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)


;; Auto indent for pasted code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-S-y") 'yank-and-indent)


;; Highlights matching parenthesis
(show-paren-mode 1)


;; Rainbow parentheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Highlight current line
(global-hl-line-mode 1)


;; Search with Ivy
(use-package swiper-helm
  :ensure t
  :diminish
  (ivy-mode counsel-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  ("C-s" . 'swiper))

;; Enter automatically indent code
(define-key global-map (kbd "RET") 'newline-and-indent)


;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; Fast comment toggle
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Join line and next line
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-^") 'top-join-line)


;; Auto complete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package pos-tip
  :ensure t)
(use-package popup
  :ensure t)

(use-package company-quickhelp
  :ensure t
  :after (company pos-tip popup)

  :config
  (company-quickhelp-mode 1)
  ;; Activate after 2 chars
  (setq company-minimum-prefix-length 2)
  ;; Zero-delay
  (setq company-idle-delay 0)

  :bind (
         ("C-\\" . 'company-complete)))



;; Flycheck
(use-package flycheck
  ;; Install back-end checker
  ;; pip install pylint
  :ensure t
  :config
  ;; Off by default
  ;; (global-flycheck-mode)
  :bind ("<f12>" . 'flycheck-mode))


;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :bind (
         ;; Undo
         ("C-/" . 'undo)
         ;; Redo
         ("C-S-/" . 'undo-tree-redo)))


;; Go to matching parenthesis
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)


(provide 'init-editing)
