;;
;; Basic
;;
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

;; Line numbers
(global-linum-mode t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Show column
(setq column-number-mode t)


;;
;; Themes
;;
;; (use-package doom-themes)
(use-package plan9-theme)
(load-theme 'plan9 t)

;; Use system font by default
(setq font-use-system-font t)

;; customize current font
;; (set-frame-font "Hack" nil t)
;; (add-to-list 'default-frame-alist
;;              '(font . "Hack"))


(provide 'core-ui)
