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


;; revert buffer but keeping mode
;; if buffer is not saved, ask for confirmation before reverting
(defun ~revert-buffer ()
  (interactive)
  (if (buffer-modified-p (current-buffer))
      (revert-buffer t nil t)
      (revert-buffer t t t)))

;;
;; Recent files
;;
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25))

;; select recent files
(defun ~list-recent-files ()
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Recent files: " files nil t))))


;;
;; Workspace management
;;
;; Why?
;; + Reduce cognitive load of buffer managements
;;
;; Ref: https://github.com/nex3/perspective-el
;;
;; History:
;; + eyebrowse: supports window layouts but not buffer lists,
;;   still easy to get lost if I have a lot of buffers opening.
;;
(use-package perspective
  :config
  (persp-mode 1))

(defun ~persp-kill-current ()
  "Kill current workpsace"
  (interactive)
  (persp-kill (persp-current-name)))

(defun ~persp-remove-current-buffer ()
  "Remove the current buffer from the current workspace"
  (interactive)
  (persp-remove-buffer (current-buffer)))

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

;; follow symblink
(setq find-file-visit-truename t)

;;
;; Collection of useful pluins
;; ref: https://github.com/Fuco1/dired-hacks
;;
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-narrow)

(provide 'core-project)
