(require 'init-elpa)

(require-package 'git-gutter+)
(require-package 'magit)

; Magit
;; git status
(global-set-key (kbd "C-x g") 'magit-status)
;; shortcuts help
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

; Git Gutter (+)
;; Enable git-gutter on all mode
(global-git-gutter+-mode)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

(provide 'init-helper)
