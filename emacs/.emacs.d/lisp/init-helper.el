(require 'init-elpa)


;; Magit
(use-package magit
  :ensure t
  :bind (;; git status
         ("C-x g" . 'magit-status)
         ;; shortcuts help
         ("C-x M-g" . 'magit-dispatch-popup)))


;; Git Gutter (+)
(use-package git-gutter
  :ensure t
  :config (global-git-gutter+-mode))


;; Enable highlighting TODO, FIXME, etc.
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))


;; Easy text scale
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))


;; Remote
(setq tramp-default-method "sshx")

;; Disable Ctrl-Z (freeze)
(global-unset-key (kbd "C-z"))


(provide 'init-helper)
