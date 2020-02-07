;;
;; Principle
;; - Be simple and mnemonic
;; - Be descriptive (e.g. show a window with hints)
;;

(require 'init-elpa)
(require 'init-language)
(require 'init-edit)

;;
;; Evil keybinding
;; ref: https://github.com/emacs-evil/evil
;;
(use-package evil
  :ensure t
  :init
  (evil-mode 1)

  :config

  ;;
  ;; Navigation
  ;;
  (global-set-key (kbd "<f3>") 'helm-find-files)
  (global-set-key (kbd "<f8>") 'helm-mini)
  (global-set-key (kbd "<f9>") 'dired-jump)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-s") 'helm-do-grep-ag)

  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)

  ;; dired
  (evil-define-key 'normal 'dired-mode-map
    "gr" 'revert-buffer)

  ;;
  ;; Edit
  ;;
  (global-set-key (kbd "<f2>") 'write-file)
  (global-set-key (kbd "M-k") 'helm-show-kill-ring)


  ;; Haskell
  (evil-define-key 'normal haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)


  ;;
  ;; Magit
  ;;
  (global-set-key (kbd "M-g") 'magit-status)
  (global-set-key (kbd "M-G") 'magit-dispatch)
)

;;
;; Evil escape
;; quickly escape to normal state using 'fd'
;; ref: https://github.com/syl20bnr/evil-escape
;;
(use-package evil-escape
  :ensure t
  :after evil
  :init
  (evil-escape-mode))

;;
;; Evil magit
;;
(use-package evil-magit
  :ensure t
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  )


(provide 'init-keys)
