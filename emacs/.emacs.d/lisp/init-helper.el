(require 'init-elpa)


;; Magit
(require-package 'magit)

;; git status
(global-set-key (kbd "C-x g") 'magit-status)
;; shortcuts help
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;; Git Gutter (+)
(require-package 'git-gutter+)

;; Enable git-gutter on all mode
(global-git-gutter+-mode)


;; Enable highlighting TODO, FIXME, etc.
(require-package 'hl-todo)
(global-hl-todo-mode)


(provide 'init-helper)
