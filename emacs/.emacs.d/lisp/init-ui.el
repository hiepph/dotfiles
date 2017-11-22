(require 'init-elpa)

(require-package 'sublime-themes)

; Themes
(load-theme 'junio t)

; Fonts

;; Line numbers
(global-linum-mode t)

; Startup
(setq inhibit-splash-screen t)
;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-startup-message t)
;; Enable transient mark mode
(transient-mark-mode 1)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

; (setq
;       x-select-enable-clipboard t
;       x-select-enable-primary t
;       save-interprogram-paste-before-kill t
;       apropos-do-all t
;       mouse-yank-at-point t)

(provide 'init-ui)
