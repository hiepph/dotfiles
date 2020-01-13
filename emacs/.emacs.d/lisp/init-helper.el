(require 'init-elpa)


;; Magit
(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo"
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))

(use-package magit
  :ensure t
  :bind
  ;; git status
  ("C-x g" . 'magit-status)
  ;; shortcuts help
  ("C-x M-g" . 'magit-dispatch-popup)
  ("C-c m d" . 'magit-diff-buffer-file)
  :init
  ;; When 'C-c C-c' is pressed in the magit commit message buffer,
  ;; delete the magit-diff buffer related to the current repo.
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-post-finish-hook
                        #'kill-magit-diff-buffer-in-current-repo
                        nil t))))


;; Git Gutter (+)
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode)
  (setq git-gutter-fr:side 'right-fringe))


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


;; Search with Ivy
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))

(use-package swiper
  :ensure t
  :diminish
  (ivy-mode counsel-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (setq ivy-display-style 'fancy)
  (advice-add 'swiper :after #'bjm-swiper-recenter)
  ;; M-c to toggle sensitive search
  (define-key ivy-minibuffer-map (kbd "M-c") 'ivy-toggle-case-fold)
  :bind
  ("C-s" . 'swiper))


;; Disable Ctrl-Z (freeze)
(global-unset-key (kbd "C-z"))


;; String manipulation
(use-package s
  :ensure t)


;; Useful extension for Emacs (Crux)
;; https://github.com/bbatsov/crux
(use-package crux
  :ensure t)

;; Buffers
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Make *scratch* buffer default to org
(setq initial-scratch-message "")
(setq initial-major-mode 'org-mode)

;; Create a new Org scratch buffer
(defun new-org-buffer ()
  (interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*org-temp*"
                              (if (= n 0) "" (int-to-string n))
                              "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode))
          ;; When called non-interactively, the `t` targets the other window (if it exists).
          (throw 'done (display-buffer buffer t))) ))))


(provide 'init-helper)
