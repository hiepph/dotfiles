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
;; Search
;; ref: https://github.com/abo-abo/swiper
;;
;; (use-package swiper
;;   :ensure t
;;   :init
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   :config
;;   (setq ivy-display-style 'fancy)

;;   (defun bjm-swiper-recenter (&rest args)
;;     "recenter display after swiper"
;;     (recenter))
;;   (advice-add 'swiper :after #'bjm-swiper-recenter)
;;   )


(provide 'init-helper)
