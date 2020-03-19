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
  :init
  (projectile-mode +1))



(provide 'core-projects)
