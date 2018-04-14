(require 'init-elpa)


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;; No need for ~ files when editing
(setq create-lockfiles nil)


;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)


;; <f5> for restart emacs
(global-set-key [f5] 'eval-buffer)


;; Auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)


;; Nyan Nyan!
(require-package 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
;; (nyan-toggle-wavy-trail)


;; Dashboard
(require-package 'dashboard)
;; (dashboard-setup-startup-hook)
;; Set the title
;; (setq dashboard-banner-logo-title "Fly me to the moon...")
;; Set the banner
;; (setq dashboard-startup-banner "~/.emacs.d/assets/yuru_camp.png")


(provide 'init-miscellaneous)
