(require 'init-elpa)


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit
  :ensure t)


;;
;; Key hints
;; ref: https://github.com/justbur/emacs-which-key
;;
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;
;; Helm
;;
(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  :config
  ;; Always stay as separated below window
  (setq helm-always-two-windows t)
  (setq helm-split-window-in-side-p t)
  (setq helm-split-window-default-side 'below)

  ;; Fuzzy match
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  ;; Auto resize to fit the number of candidates
  (helm-autoresize-mode t)
  )


;;
;; Virtual desktop
;;
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  )


;;
;; Project management
;;
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  )


(provide 'init-helper)
