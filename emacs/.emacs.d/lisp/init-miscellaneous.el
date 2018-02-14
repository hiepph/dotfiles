(require 'init-elpa)

(require-package 'nyan-mode)
(require-package 'dashboard)


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; <f5> for restart emacs
(global-set-key [f5] 'eval-buffer)

;; Nyan! at startup
(nyan-mode)
(nyan-start-animation)
;; (nyan-toggle-wavy-trail)

;; Dashboard
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Fly me to the moon...")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/assets/yuru_camp.png")

(provide 'init-miscellaneous)
