(require 'init-elpa)

;; Themes
;; Some external themes
(let ((themes '(dracula-theme
                atom-one-dark-theme
                doom-themes)))
  (dolist (theme themes) (unless (package-installed-p theme)
                            (package-install theme))))

;; Specify folder contains themes
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


;; Look & Appearance
(let ((looks '(dracula-theme
                atom-one-dark-theme
                doom-themes)))
  (dolist (look looks) (unless (package-installed-p look)
                           (package-install theme))))

;; Basic
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Disable startup message
(setq inhibit-startup-message t)

;; Disable Menu/toolbar/scroll
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Line numbers
(global-linum-mode t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Show column
(setq column-number-mode t)

;; Save customization
; (setq custom-file "~/.emacs.d/custom.el")
; (load custom-file)
; (add-hook 'kill-emacs-query-functions
;           'custom-prompt-customize-unsaved-options)


;; Tree
(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init (progn
          ;; pull all fonts/icons
          ;; (all-the-icons-install-fonts)
          ;; enable icons
          ;; (all-the-icons-icon-for-buffer)
          ;; (all-the-icons-icon-for-file)
          ;; (all-the-icons-icon-for-mode)

          ;; slow rendering
          (setq inhibit-compacting-font-caches t)

          ;; set icons theme
          (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

          ;; Every time when the neotree window is opened, let it find current file and jump to node
          (setq neo-smart-open t)

          ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
          (setq projectile-switch-project-action 'neotree-projectile-action)

          ;; show hidden files
          (setq-default neo-show-hidden-files t)))

;; indent guide
(use-package indent-guide
  :ensure t
  :defer t
  :config (indent-guide-global-mode))


;; (require-package 'tabbar-ruler)
;; (require-package 'all-the-icons)

;; Tabbar
(use-package tabbar-ruler
  :ensure t
  :init (progn
          (setq tabbar-ruler-global-tabbar t)    ; get tabbar
          ;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
          ;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
          ;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
          ;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
          )
  :config (tabbar-ruler-group-by-projectile-project)
  :bind (("C-c t" . 'tabbar-ruler-move)
         ("C-<" . 'tabbar-ruler-backward)
         ("C->" . 'tabbar-ruler-forward)
         ("C-S-p" . 'tabbar-ruler-tabbar-backward-group)
         ("C-S-n" . 'tabbar-ruler-tabbar-forward-group)))

;; sexy mode line
(use-package smart-mode-line
  :ensure t
  :config (sml/setup))


(provide 'init-ui)
