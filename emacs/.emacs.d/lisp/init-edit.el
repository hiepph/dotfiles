(require 'init-elpa)
;; (require 'init-func)


;;
;; Basic
;;

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
;; Pairs
;;

;; Highlights matching parenthesis
(show-paren-mode 1)


;; Autopair
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t))


;; Expand region
(use-package expand-region
  :ensure t)


;; Rainbow parentheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))




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





;; ;; Fast comment toggle
;; (defun toggle-comment-on-line ()
;;   "Comment or uncomment current line."
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;; (global-set-key (kbd "C-;") 'toggle-comment-on-line)


;; ;; Auto complete
;; (use-package company
;;   :ensure t
;;   :hook
;;   (prog-mode . company-mode)
;;   :config
;;   ;; case sensitive completion
;;   (setq company-dabbrev-downcase nil)

;;   ;; dabbrev completion (https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html)
;;   (add-to-list 'company-backends '(company-capf company-dabbrev))

;;   ;; fuzzy matching
;;   (setq company-require-match nil)  ; Don't require match, so you can still move your cursor as expected.
;;   (setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
;;   (setq company-eclim-auto-save nil)          ; Stop eclim auto save.
;;   (setq company-dabbrev-downcase nil)         ; No downcase when completion.

;;   ;; Enable downcase only when completing the completion.
;;   (defun jcs--company-complete-selection--advice-around (fn)
;;     "Advice execute around `company-complete-selection' command."
;;     (let ((company-dabbrev-downcase t))
;;       (call-interactively fn)))
;;   (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)
;;   )

;; (use-package pos-tip
;;   :ensure t)
;; (use-package popup
;;   :ensure t)

;; (use-package company-quickhelp
;;   :ensure t
;;   :after (company pos-tip popup)

;;   :config
;;   (company-quickhelp-mode 1)
;;   ;; Activate after 2 chars
;;   (setq company-minimum-prefix-length 2)
;;   ;; Zero-delay
;;   (setq company-idle-delay 0)

;;   :bind
;;   ("C-\\" . 'company-complete))


;; ;; Flycheck
;; (use-package flycheck
;;   ;; Install back-end checker
;;   ;; pip install pylint
;;   :ensure t
;;   :hook
;;   (prog-mode . flycheck-mode)
;;   :config
;;   (setq-default flycheck-emacs-lisp-load-path 'inherit)
;;   (setq flycheck-flake8-maximum-line-length 120)
;;   (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00"))
;;   (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c"))
;;   (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen"))

;;   ;; theme
;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
;;     (vector #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00111000
;;             #b01111100
;;             #b11111110
;;             #b11111110
;;             #b01111100
;;             #b00111000
;;             #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00000000
;;             #b00000000))

;;   (flycheck-define-error-level 'error
;;     :severity 100
;;     :compilation-level 2
;;     :overlay-category 'flycheck-error-overlay
;;     :fringe-bitmap 'flycheck-fringe-bitmap-ball
;;     :fringe-face 'flycheck-fringe-error
;;     :error-list-face 'flycheck-error-list-error)

;;   ;; check only when save file or change the major mode
;;   (setq flycheck-check-syntax-automatically '(save mode-enable)))


;; ;; Undo tree
;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (global-undo-tree-mode 1)
;;   :bind
;;   ;; Undo
;;   ("C-/" . 'undo)
;;   ;; Redo
;;   ("C-S-/" . 'undo-tree-redo))


;; ;; Go to matching parenthesis
;; (defun goto-match-paren (arg)
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))


;; ;; Multiple cursors
;; (use-package multiple-cursors
;;   :ensure t
;;   :config
;;   (global-unset-key (kbd "C-<down-mouse-1>"))
;;   (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))


;; ;; ACME
;; (use-package wand
;;   :ensure t
;;   :bind
;;   ([mouse-2] . 'wand:execute)
;;   ("C-<return>" . 'wand:execute)
;;   :config
;;   (setq wand:*rules*
;;         (list
;;          (wand:create-rule :match (rx bol (0+ " ") "$")
;;                            :capture :after
;;                            :action #'~acmec)
;;          (wand:create-rule :match (rx bol (0+ " ") "<")
;;                            :capture :after
;;                            :action #'~acme<)
;;          (wand:create-rule :match (rx bol (0+ " ") "http")
;;                            :capture :whole
;;                            :action #'browse-url-firefox)
;;          )))


(provide 'init-edit)
