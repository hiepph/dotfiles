(require 'init-elpa)

(require-package 'git-gutter+)
(require-package 'magit)
(require-package 'hl-todo)


;; Magit
;; git status
(global-set-key (kbd "C-x g") 'magit-status)
;; shortcuts help
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;; Git Gutter (+)
;; Enable git-gutter on all mode
(global-git-gutter+-mode)


;; Enable highlighting TODO, FIXME, etc.
(global-hl-todo-mode)
;; Jump to next/prev phrases
(define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)


(provide 'init-helper)
