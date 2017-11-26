(require 'init-elpa)

(require-package 'neotree)
(require-package 'all-the-icons)
(require-package 'dracula-theme)

; Basic
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

; Themes
(load-theme 'dracula t)

; Fonts
(set-frame-font "Fira Mono for Powerline" nil t)

; Tree
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
;; (setq-default neo-show-hidden-files t)

(provide 'init-ui)
