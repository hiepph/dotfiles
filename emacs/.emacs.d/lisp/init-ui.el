(require 'init-elpa)

(require-package 'sublime-themes)

;; Themes
(load-theme 'junio t)

;; Fonts

;; Line numbers
(global-linum-mode t)

;; Startup
(setq inhibit-startup-message t)
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
