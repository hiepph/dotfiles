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


;; Easy text scale
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))


(provide 'init-helper)
