;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Disable startup message
(setq inhibit-startup-message t)

;; *scratch* message
(setq initial-scratch-message "")

;; Disable Menu/toolbar/scroll
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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


;; Enable variable pitch mode will help rendering mixed fonts
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;;
;; Custom fonts for some specific modes
;;
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq buffer-face-mode-face (font-get (face-attribute 'default :font) :family))))
(add-hook 'git-commit-mode-hook
          (lambda ()
            (setq buffer-face-mode-face (font-get (face-attribute 'default :font) :family))))


;;
;; Theme
;;

;; (straight-use-package
;;  '(colorless-theme :type git
;;                    :repo "https://git.sr.ht/~lthms/colorless-themes.el"
;;                    :branch "master"))
;; (load-theme 'einkless t)

(use-package plan9-theme)
(load-theme 'plan9 t)


;;
;; ORG appearance
;; Font can be find here: https://github.com/edwardtufte/et-book
;;
(let* ((org-text-font "ETBembo:style=Regular")
       (org-code-font (format "%s" (font-get (face-attribute 'default :font) :family)))

       (variable-tuple
        (cond
         ((x-list-fonts org-text-font) `(:font ,org-text-font))
         (nil (warn "Cannot find a suitable font."))))
       (base-font-color (face-foreground 'default nil 'default))
       (headline `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user

     ;; Variable height for headings and list
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))

     ;; Set font here

     ;; Text and symbols: slightly bigger for readability
     `(variable-pitch ((t (:family ,org-text-font :height 135))))
     ;; Code, example blocks: regular weight for contrast
     `(fixed-pitch ((t (:family ,org-code-font :weight normal :height 120))))

     ;; At least I need to distinguish between text and code
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
     '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

;; increase sizes of LaTeX fragments
(plist-put org-format-latex-options :scale 1.5)

(eval-after-load 'org
  '(setf org-highlight-latex-fragments-and-specials t))

;;
;; Better scroll bar
;; ref: https://github.com/emacsorphanage/yascroll
;;
(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))


;;
;; Modeline
;;
(use-package telephone-line
  :config
  (telephone-line-mode 1))


(provide 'core-ui)
