;; Principles:
;; 1. Mnemonic: c(ompile), b(uffers)
;; 2. Only high frequency tasks are bind to F keys

;; References
;; 1. Spacemacs: https://develop.spacemacs.org/doc/DOCUMENTATION.html

;;
;; General (leader keys)
;; ref: https://github.com/noctuid/general.el/
;;
(use-package general
  :config
  ;;
  ;; Frequent tasks
  ;;
  (global-set-key (kbd "<f2>") 'save-buffer)
  (global-set-key (kbd "<f3>") 'counsel-find-file)
  (global-set-key (kbd "<f8>") 'counsel-switch-buffer)
  (global-set-key (kbd "<f9>") '~compile-current-file)
  (global-set-key (kbd "<f12>") '~test-current-file)
  (global-set-key (kbd "S-<f12>") '~test-all-files)

  ;;
  ;; Alt combination
  ;;
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-<return>") 'wand:execute)

  (global-set-key (kbd "M-&") 'async-shell-command)
  (global-set-key (kbd "M-|") '~acme|)
  (global-set-key (kbd "M-$") '~acme$)
  (global-set-key (kbd "M-<") '~acme<)

  ;; Window movement
  ;; In replace for evil C-w + <> which I found time consuming
  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)

  (general-evil-setup)
  (general-nmap
    :prefix "SPC"

    ;; Commands
    "x" 'counsel-M-x

    ;; Magit
    "g" 'magit-status
    "G" 'magit-dispatch

    ;; quick search
    "/" 'counsel-rg

    ;; registers
    "r" 'counsel-evil-registers
    "R" 'counsel-yank-pop

    ;; marks
    "m" 'counsel-evil-marks

    ;; dired
    "d" 'dired-jump

    ;; Projectile
    "p" 'projectile-command-map

    ;; Perspective
    "w" 'persp-key-map)


  ;;
  ;; buffers
  ;;
  (general-nmap
    :prefix "SPC b"

    "!" 'counsel-switch-to-shell-buffer

    "k" '~kill-current-buffer
    "m" 'magit-diff-buffer-file)

  ;;
  ;; compile
  ;;
  (general-nmap
    :prefix "SPC c"
    "c" 'compile
    "r" '~recompile
    "k" 'kill-compilation)

  ;;
  ;; dired
  ;;
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" 'dired-subtree-toggle)

  ;;
  ;; (t)oggle mode
  ;;
  (general-define-key
   :states 'normal
   :prefix "SPC t"

   "e" 'flycheck-mode)

  ;;
  ;; (e)rror handling
  ;;
  (general-define-key
   :states 'normal
   :prefix "SPC e"

   "l" 'flycheck-list-errors
   "!" 'flycheck-display-error-at-point
   "c" 'flycheck-clear
   "b" 'flycheck-buffer
   "n" 'flycheck-next-error
   "p" 'flycheck-previous-error
   "y" 'flycheck-copy-errors-as-kill)

  ;;
  ;; Languages
  ;;
  ;; Haskell
  (general-define-key
   :states 'normal
   :keymaps 'haskell-mode-map

   "O" 'haskell-evil-open-above)

  ;; Emacs Lisp
  (general-define-key
   :states 'normal
   :keymaps 'emacs-lisp-mode-map

   [f5] '~eval-buffer)

  ;;
  ;; Editor
  ;;
  ;; ivy
  (general-define-key
   :states 'normal
   "\\" 'swiper)

  ;; expand-region
  (general-define-key
   :states 'visual
   "+" 'er/expand-region
   "-" 'er/contract-region)

  ;; paredit
  (general-define-key
   :states 'normal
   :keymaps 'paredit-mode-map

   ">" 'paredit-forward-slurp-sexp
   "<" 'paredit-forward-barf-sexp))

(provide 'core-keybindings)
