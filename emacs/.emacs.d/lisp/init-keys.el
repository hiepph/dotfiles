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
  (general-evil-setup)

  ;;
  ;; Frequent tasks
  ;;
  (global-set-key (kbd "<f2>") 'write-file)
  (global-set-key (kbd "<f3>") 'helm-find-files)
  (global-set-key (kbd "<f5>") 'eval-buffer)
  (global-set-key (kbd "<f8>") 'helm-mini)
  (global-set-key (kbd "<f9>") 'recompile)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-<return>") 'wand:execute)

  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)

  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-j") 'windmove-down)


  ;;
  ;; SPC keys leader
  ;;
  (general-nmap
   :prefix "SPC"

   ;; Commands
   "x" 'helm-M-x

   ;; Magit
   "g" 'magit-status
   "G" 'magit-dispatch

   ;;
   ;; Navigation
   ;;
   ;; search
   "/" 'helm-do-grep-ag

   ;; dired
   "d" 'dired-jump

   ;; Projectile
   "p" 'projectile-command-map

   ;; Dired mode
   "d" 'dired-jump

   ;;
   ;; Edit
   ;;
   ;; show kill ring
   "k" 'helm-show-kill-ring
   ))


(provide 'init-keys)
