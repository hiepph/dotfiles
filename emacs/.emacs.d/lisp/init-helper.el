(require 'init-elpa)

(require-package 'git-gutter)
(require-package 'magit)

; Magit
;; git status
(global-set-key (kbd "C-x g") 'magit-status)
;; shortcuts help
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

; Git Gutter
;; Enable git-gutter on all mode
(global-git-gutter-mode +1)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; reload after magit
(defvar my-magit-after-stage-hooks nil
  "Hooks to be run after staging one item in magit.")

(defvar my-magit-after-unstage-hooks nil
  "Hooks to be run after unstaging one item in magit.")

(defadvice magit-stage-item (after run-my-after-stage-hooks activate)
  "Run `my-magit-after-stage-hooks` after staging an item in magit."
  (when (called-interactively-p 'interactive)
    (run-hooks 'my-magit-after-stage-hooks)))

(defadvice magit-unstage-item (after run-my-after-unstage-hooks activate)
  "Run `my-magit-after-unstage-hooks` after unstaging an item in magit."
  (when (called-interactively-p 'interactive)
    (run-hooks 'my-magit-after-unstage-hooks)))

(defun my-refresh-visible-git-gutter-buffers ()
  "Refresh git-gutter-mode on all visible git-gutter-mode buffers."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter-mode (get-buffer-window buff))
        (git-gutter-mode t)))))

(add-hook 'my-magit-after-unstage-hooks
          'my-refresh-visible-git-gutter-buffers)
(add-hook 'my-magit-after-stage-hooks
          'my-refresh-visible-git-gutter-buffers)

(provide 'init-helper)
