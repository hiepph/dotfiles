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

  ;; Set Helm to use `ripgrep`
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
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
