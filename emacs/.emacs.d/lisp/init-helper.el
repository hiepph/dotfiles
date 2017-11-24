(require 'init-elpa)

(require-package 'git-gutter)
(require-package 'magit)

; Magit
(global-set-key (kbd "C-x g") 'magit-status)

; Git Gutter
;; Enable git-gutter on all mode
(global-git-gutter-mode +1)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

(provide 'init-helper)
