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
  (global-set-key (kbd "S-<f2>") 'write-file)
  (global-set-key (kbd "<f3>") 'counsel-find-file)
  (global-set-key (kbd "S-<f3>") 'find-file-other-window)
  (global-set-key (kbd "<f8>") 'counsel-switch-buffer)
  (global-set-key (kbd "S-<f8>") 'counsel-switch-buffer-other-window)
  (global-set-key (kbd "<f9>") '~compile-current-file)
  (global-set-key (kbd "S-<f9>") '~recompile)
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

  (global-set-key (kbd "M-&") 'async-shell-command)
  (global-set-key (kbd "M-|") '~acme|)
  (global-set-key (kbd "M-$") '~acme$)
  (global-set-key (kbd "M-<") '~acme<)

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

    ;; dired
    "d" 'dired-jump

    ;; kill-ring
    "y" 'counsel-yank-pop

    ;; Projectile
    "p" 'projectile-command-map)


  ;; buffers
  (general-nmap
    :prefix "SPC b"

    "k" '~kill-current-buffer
    "K" '~kill-buffer

    "m" 'magit-diff-buffer-file)

  ;; compile
  (general-nmap
    :prefix "SPC c"
    "c" 'compile
    "C" '~recompile
    "k" 'kill-compilation)

  ;; dired
  (general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "TAB" 'dired-subtree-toggle)

  ;; Haskell
  (general-define-key
   :states 'normal
   :keymaps 'haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)

  ;; Clojure
  (general-define-key
   :states 'normal
   :keymaps 'cider-mode-map
   :prefix "SPC r"
   "l" 'cider-load-buffer
   "L" 'cider-load-buffer-and-switch-to-repl-buffer)

  ;; Emacs Lisp
  (general-define-key
   :states 'normal
   :keymaps 'emacs-lisp-mode-map

   [f5] '~eval-buffer)

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
