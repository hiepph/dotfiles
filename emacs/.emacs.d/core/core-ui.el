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
(global-display-line-numbers-mode)

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
;; ref: https://github.com/antonj/Highlight-Indentation-for-Emacs
;;
(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

(add-hook 'python-mode-hook 'highlight-indentation-mode)

;; Use system font by default
(setq font-use-system-font t)

;; Transparent title bar (e.g. for MacOS)
(add-to-list
 'default-frame-alist'(ns-transparent-titlebar . t))

(add-to-list
 'default-frame-alist'(ns-appearance . light))


;; Enable variable pitch mode will help rendering mixed fonts
(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;;
;; Manage popup windows
;; ref: https://github.com/karthink/popper
;;
(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*helpful.*"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1))


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
(let* ((org-text-font "ETBembo")
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
