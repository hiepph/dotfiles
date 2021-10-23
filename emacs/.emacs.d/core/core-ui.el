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

;; Show column
(setq column-number-mode t)

;;
;; Indent line
;; ref: https://github.com/zk-phi/indent-guide
;;
(use-package indent-guide)


;; Use system font by default
(setq font-use-system-font t)

;;
;; Mode line
;;
(use-package mood-line
  :init
  (mood-line-mode 1))


(provide 'core-ui)
