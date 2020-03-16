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
  ;; Haskell
  (evil-define-key 'normal haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)

  ;; dired
  (evil-define-key 'normal 'dired-mode-map
    "gr" 'revert-buffer)

  (with-eval-after-load 'evil-maps
    ;; swiper
    (define-key evil-motion-state-map (kbd "/") 'swiper)
    )
)

;;
;; Evil escape
;; quickly escape to normal state using 'fd'
;; ref: https://github.com/syl20bnr/evil-escape
;;
(use-package evil-escape
  :ensure t
  :after evil
  :diminish
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



;;
;; General (leader keys)
;; ref: https://github.com/noctuid/general.el/
;;
(use-package general
  :ensure t
  :config
  ;;
  ;; Frequent tasks
  ;;
  (global-set-key (kbd "<f1>") 'shell)
  (global-set-key (kbd "<f2>") 'write-file)
  (global-set-key (kbd "<f3>") 'counsel-find-file)
  (global-set-key (kbd "<f5>") 'eval-buffer)
  (global-set-key (kbd "<f8>") 'counsel-switch-buffer)
  (global-set-key (kbd "S-<f8>") 'counsel-switch-buffer-other-window)
  (global-set-key (kbd "<f9>") '~compile-current-file)
  (global-set-key (kbd "<f12>") '~test-current-file)
  (global-set-key (kbd "S-<f12>") '~test-all-files)


  ;;
  ;; Alt combination
  ;;
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-<return>") 'wand:execute)

  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)

  ;; (global-set-key (kbd "M-h") 'windmove-left)
  ;; (global-set-key (kbd "M-l") 'windmove-right)
  ;; (global-set-key (kbd "M-k") 'windmove-up)
  ;; (global-set-key (kbd "M-j") 'windmove-down)

  (global-set-key (kbd "M-!") '~acme!)
  (global-set-key (kbd "M-|") '~acme|)
  (global-set-key (kbd "M-$") '~acme$)


  ;;
  ;; SPC keys leader
  ;;
  (general-evil-setup)
  (general-nmap
   :prefix "SPC"

   ;; Commands
   "x" 'counsel-M-x
   "k" 'kill-buffer

   ;; Magit
   "g" 'magit-status
   "G" 'magit-dispatch

   ;;
   ;; Navigation
   ;;
   ;; search
   "/" 'counsel-rg

   ;; dired
   "d" 'dired-jump

   ;; Projectile
   "p" 'projectile-command-map
   ))


(provide 'init-keys)
