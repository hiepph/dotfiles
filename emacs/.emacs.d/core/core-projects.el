;;
;; dired
;; ref: https://github.com/Fuco1/dired-hacks
;;
;; (use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)

;; readable information
(setq dired-listing-switches "-alh")

;; auto revert dired-mode
(add-hook 'dired-mode-hook 'auto-revert-mode)


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit)


;;
;; Key hints
;; ref: https://github.com/justbur/emacs-which-key
;;
(use-package which-key
  :init
  (which-key-mode))


;;
;; Selectrum
;; ref: https://github.com/raxod502/selectrum
;; incremental narrowing search
;;
(use-package selectrum
  :init
  (selectrum-mode +1))

;;
;; Prescient
;; ref: https://github.com/raxod502/prescient.el
;; save command history on disk,
;; so the sorting gets more intelligent over time
;;
(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package company-prescient
  :init
  (company-prescient-mode +1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1))

;;
;; Desktops management
;;
(use-package eyebrowse
  :diminish
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))


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
;; Project management
;;
(use-package projectile
  :config
  (projectile-mode +1)
  ;; ivy interface
  (setq projectile-completion-system 'default)

  ;; sort files by recently opened
  (setq projectile-sort-order 'recentf)

  ;; open top-level directory instead of a specific files
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )

;;
;; Jump to definition, even without CTAGS
;; ref: https://github.com/jacktasia/dumb-jump
;;
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(provide 'core-projects)
