;;
;; Buffers
;;

;; Auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)

(defun ~eval-buffer ()
  "eval-buffer, with message"
  (interactive)
  (eval-buffer)
  (message "> Eval buffer succeeded"))

(defun ~kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ~kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; open *shell* in current buffer
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; auto truncate buffers at 1024 lines
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)


;;
;; Recent files
;;
(use-package recentf
  :config
  (recentf-mode 1))

;; select recent files
(defun ~list-recent-files ()
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Recent files: " files nil t))))


;;
;; Desktops management
;;
(use-package eyebrowse
  :diminish
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))


;;
;; Project management
;;
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)

  ;; sort files by recently opened
  (setq projectile-sort-order 'recentf)

  ;; open top-level directory instead of a specific files
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )


;;
;; dired
;;

;; enable `dired-find-alternate-file' immediately without asking
(put 'dired-find-alternate-file 'disabled nil)

;; auto revert dired-mode
(setq dired-auto-revert-buffer t)

;; auto hide dot files
(add-hook 'dired-mode-hook 'dired-filter-by-dot-files)

;; make Dired guess the default target for commands like copy/move
(setq dired-dwim-target t)

;;
;; Collection of useful pluins
;; ref: https://github.com/Fuco1/dired-hacks
;;
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-narrow)

(provide 'core-project)