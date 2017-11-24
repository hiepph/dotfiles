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

(provide 'init-helper)
