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
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t))


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


(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode)
  (set-face-background 'indent-guide-face "dimgray")
  (setq indent-guide-delay 0.1))


;; Highlights matching parenthesis
(show-paren-mode 1)


;; Rainbow parentheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Highlight current line
(global-hl-line-mode 1)


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
  (add-hook 'after-init-hook 'global-company-mode)
  ;; case sensitive completion
  (setq company-dabbrev-downcase nil)

  ;; dabbrev completion (https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html)
  (add-to-list 'company-backends '(company-capf company-dabbrev))

  ;; fuzzy matching
  (setq company-require-match nil)  ; Don't require match, so you can still move your cursor as expected.
  (setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
  (setq company-eclim-auto-save nil)          ; Stop eclim auto save.
  (setq company-dabbrev-downcase nil)         ; No downcase when completion.
  :config
  ;; Enable downcase only when completing the completion.
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))

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

  :bind
  ("C-\\" . 'company-complete))



;; Flycheck
(use-package flycheck
  ;; Install back-end checker
  ;; pip install pylint
  :ensure t
  :config
  ;; off by default
  (global-flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-flake8-maximum-line-length 120)
  (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00"))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c"))
  (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen"))

  ;; theme
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  ;; check only when save file or change the major mode
  (setq flycheck-check-syntax-automatically '(save mode-enable))

  :bind ("<f12>" . 'flycheck-mode))


;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :bind
  ;; Undo
  ("C-/" . 'undo)
  ;; Redo
  ("C-S-/" . 'undo-tree-redo))


;; Go to matching parenthesis
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C-S-f" . 'mc/mark-next-like-this)
  ("C-S-b" . 'mc/mark-previous-like-this)
  ("C-S-d" . 'mc/mark-all-like-this)

  :config
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))


(provide 'init-editing)
