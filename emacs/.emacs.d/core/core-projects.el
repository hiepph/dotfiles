;;
;; dired
;; ref: https://github.com/Fuco1/dired-hacks
;;
;; (use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)

;; readable information
(setq dired-listing-switches "-alh")

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (dired-filter-mode 1)))



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
;; Counsel & Ivy & Swiper
;; ref:
;; https://sam217pa.github.io/2016/093/from-helm-to-ivy/
;; https://github.com/abo-abo/swiper
;;
;; docs: https://oremacs.com/swiper/
;;
(use-package counsel
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))


;;
;; Virtual desktop
;;
(use-package eyebrowse
  :diminish
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  )


;;
;; Project management
;;
(use-package projectile
  :config
  (projectile-mode +1)
  ;; ivy interface
  (setq projectile-completion-system 'ivy)
  ;; sort files by recently opened
  (setq projectile-sort-order 'recentf)
  ;; open top-level directory instead of a specific files
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'core-projects)
