;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Disable startup message
(setq inhibit-startup-message t)

;; *scratch* message
(setq initial-scratch-message "")

;; Disable Menu/toolbar/scroll
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable annoying beep
(setq ring-bell-function 'ignore)

;; Line numbers
(global-linum-mode t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Text will fill to the full width of window
(add-hook 'text-mode-hook 'visual-line-mode)

;;
;; Show column
;;
(setq column-number-mode t)
(define-globalized-minor-mode fci-global-mode fci-mode (lambda () (fci-mode 1)))

;;
;; Indent line
;; ref: https://github.com/zk-phi/indent-guide
;;
(use-package indent-guide)

;; Use system font by default
(setq font-use-system-font t)

;; Transparent title bar (e.g. for MacOS)
(add-to-list
  'default-frame-alist'(ns-transparent-titlebar . t))

(add-to-list
  'default-frame-alist'(ns-appearance . light))

;;
;; Mode line
;;
(use-package mood-line
  :init
  (mood-line-mode 1))

;; Enable variable pitch mode will help rendering mixed fonts
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;;
;; Theme
;;
(straight-use-package
 '(colorless-theme :type git
                   :repo "https://git.sr.ht/~lthms/colorless-themes.el"
                   :branch "master"))
(load-theme 'einkless t)


;;
;; Fonts
;;
(set-frame-font "Input Mono" nil t)
(add-to-list 'default-frame-alist '(font . "Input Mono"))
(add-hook 'yaml-mode-hook (lambda ()
                           (setq buffer-face-mode-face '(:family "Input Mono"))
                           (buffer-face-mode)))


;;
;; ORG appearance
;;

;; Variable height for headings and list
(let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo") '(:font "ETBembo"))
                ((x-family-fonts "Input Sans") '(:font "Input Sans"))
                (nil (warn "Cannot find a suitable font."))))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; Fancy font for reading Org
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 160 :weight thin))))
 '(fixed-pitch ((t (:family "Input Mono" :height 130 :weight regular))))
 )

;; At least I need to distinguish between text and code
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(provide 'core-ui)
