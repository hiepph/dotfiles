(require 'init-elpa)

(require-package 'neotree)
(require-package 'indent-guide)
(require-package 'all-the-icons)
(require-package 'dracula-theme)
(require-package 'atom-one-dark)


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


;; Themes
;; Specify folder contains themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Choose theme here
;; (load-theme 'dracula t)
(load-theme 'atom-one-dark t)

;; Fonts
(set-frame-font "Fira Mono" nil t)

;; Tree
(global-set-key [f8] 'neotree-toggle)
;; pull all fonts/icons
                                        ; (all-the-icons-install-fonts)
;; enable icons
                                        ; (all-the-icons-icon-for-buffer)
                                        ; (all-the-icons-icon-for-file)
                                        ; (all-the-icons-icon-for-mode)

;; slow rendering
(setq inhibit-compacting-font-caches t)

;; set icons theme
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Every time when the neotree window is opened, let it find current file and jump to node
(setq neo-smart-open t)

;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
(setq projectile-switch-project-action 'neotree-projectile-action)

;; show hidden files
(setq-default neo-show-hidden-files t)

;; indent guide
(indent-guide-global-mode)

(provide 'init-ui)
