(require 'init-elpa)

(require-package 'neotree)
(require-package 'tabbar-ruler)
(require-package 'indent-guide)
(require-package 'all-the-icons)


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
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Choose theme here
(require-package 'dracula-theme)
(require-package 'atom-one-dark-theme)

(add-hook 'after-init-hook (lambda () (load-theme 'atom-one-dark)))


;; Fonts
;; (set-frame-font "Fira Mono" nil t)


;; Tree
(global-set-key [f8] 'neotree-toggle)
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
(setq-default neo-show-hidden-files t)


;; indent guide
(indent-guide-global-mode)


;; Tabbar
(setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
(require 'tabbar-ruler)
(tabbar-ruler-group-by-projectile-project)

;; shorcut keys
(global-set-key (kbd "C-c t") 'tabbar-ruler-move)
(global-set-key (kbd "C-<") 'tabbar-ruler-backward)
(global-set-key (kbd "C->") 'tabbar-ruler-forward)
(global-set-key (kbd "C-S-p") 'tabbar-ruler-tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-ruler-tabbar-forward-group)


(provide 'init-ui)
