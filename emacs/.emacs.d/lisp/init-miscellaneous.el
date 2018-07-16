(require 'init-elpa)


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;; No need for ~ files when editing
(setq create-lockfiles nil)


;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)


;; Auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)


(provide 'init-miscellaneous)
