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
;; Counsel & Ivy & Swiper
;; ref:
;; https://sam217pa.github.io/2016/093/from-helm-to-ivy/
;; https://github.com/abo-abo/swiper
;;
;; docs: https://oremacs.com/swiper/
;;
(use-package counsel
  :ensure t
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )



;;
;; Virtual desktop
;;
(use-package eyebrowse
  :ensure t
  :diminish
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  )


;;
;; Windmove
;;
(use-package windmove
  :diminish
  :ensure t)


;;
;; Project management
;;
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  )


(provide 'init-helper)
