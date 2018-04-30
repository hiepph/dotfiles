(require 'init-elpa)

(require-package 'org)
(require-package 'markdown-mode)


;; Org-mode
(add-hook 'org-mode-hook 'turn-on-font-lock)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done 'time)

;; Turn off auto-fold
(setq org-startup-folded nil)


;; For Markdown preview
;; npm install -g livedown
;; livedown start README.md --open


;; Remote
(setq tramp-default-method "sshx")


(provide 'init-mode)
