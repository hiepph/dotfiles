(require 'init-elpa)


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
;; Automatic indentation offset detection
;;
(use-package dtrt-indent
  :ensure t
  :init
  (dtrt-indent-mode)
  )

;;
;; Pairs
;;

;; Highlights matching parenthesis
(show-paren-mode 1)


;; Autopair
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode))


;; Expand region
(use-package expand-region
  :ensure t)


;; Rainbow parentheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Surround
;; ref: https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


;; Search for selected region
(use-package evil-visualstar
  :ensure t
  :init
  (global-evil-visualstar-mode))



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

;;
;; Commenter
;; ref: https://github.com/linktohack/evil-commentary
;;
(use-package evil-commentary
  :ensure t
  :init
  (evil-commentary-mode)
  )


;;
;; Undo/Redo tree
;;
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;
;; Multiple cursors
;;
;; (use-package multiple-cursors
;;   :ensure t
;;   :config
;;   ;; able to use mouse to select
;;   (global-unset-key (kbd "C-<down-mouse-1>"))
;;   (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))



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


(provide 'init-edit)
