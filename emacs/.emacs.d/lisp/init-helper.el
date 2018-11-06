(require 'init-elpa)


;; Magit
(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo"
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))

(use-package magit
  :ensure t
  :bind (;; git status
         ("C-x g" . 'magit-status)
         ;; shortcuts help
         ("C-x M-g" . 'magit-dispatch-popup))
  :config
  ;; When 'C-c C-c' is pressed in the magit commit message buffer,
  ;; delete the magit-diff buffer related to the current repo.
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-post-finish-hook
                        #'kill-magit-diff-buffer-in-current-repo
                        nil t))))


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
