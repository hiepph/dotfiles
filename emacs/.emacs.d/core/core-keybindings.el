;; Principles:
;; 1. Mnemonic: c(ompile), b(uffers)
;; 2. Only high frequency tasks are bind to F keys

;; References
;; 1. Spacemacs: https://develop.spacemacs.org/doc/DOCUMENTATION.html

;;
;; Menus
;;
(use-package hydra)
(defhydra hydra-main (:columns 4 :exit t)
  "Main"
  ;; Commands
  ("x" counsel-M-x "M-x")

  ;; buffers
  ("b" hydra-buffers/body "buffers")

  ;; dired
  ("d" dired-jump "dired")

  ;; Magit
  ("g" magit-status "magit")

  ;; Projectile
  ("p" hydra-projectile/body "projectile")

  ;; Perspective
  ("w" hydra-persp/body "workspaces")

  ;; compile
  ("c" hydra-compile/body "compile")
  ("e" hydra-error/body "flycheck")

  ;; quick search
  ("/" counsel-rg "ripgrep")

  ;; registers
  ("r" counsel-evil-registers "registers")
  ("k" counsel-yank-pop "kill rings")

  ;; marks
  ("m" counsel-evil-marks "marks"))

(defhydra hydra-projectile (:columns 4 :exit t)
  "Projectile"
  ("p"   projectile-switch-project "Switch Project")
  ("f"   projectile-find-file "Find File")
  ("r"   projectile-recentf  "Recent Files")
  ("/"   projectile-ripgrep "Ripgrep")
  ("d"   projectile-find-dir "Find Directory")
  ("b"   projectile-switch-to-buffer "Switch to Buffer")
  ("k"   projectile-kill-buffers "Kill Buffers")
  ("R"   projectile-regenerate-tags "Regenerate tags")
  ("c"   projectile-compile-project "Compile in root")
  ("!"   projectile-run-shell-command-in-root "Shell cmd in root")
  ("&"   projectile-run-async-shell-command-in-root "Async cmd in root")
  ("q"   nil "Cancel" :color blue))

(defhydra hydra-persp (:columns 4 :exit t)
  "Perspective"
  ("n" persp-next "Next")
  ("p" persp-prev "Prev")
  ("s" persp-switch "Switch")
  ("q" nil "Quit"))

(defhydra hydra-buffers (:columns 4 :exit t)
  "Buffers"
  ("!" counsel-switch-to-shell-buffer "shell")
  ("b" counsel-switch-buffer "shell")
  ("k" ~kill-current-buffer "kill current buffer")
  ("d" magit-diff-buffer-file "diff current buffer"))

(defhydra hydra-compile (:columns 4 :exit t)
  "Compile"
  ("c" compile "compile")
  ("r" ~recompile "recompile")
  ("k" kill-compilation "kill"))

(defhydra hydra-error (:columns 4 :exit t)
  "Error handling "
  ("l" flycheck-list-errors "list")
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "previous")

  ("!" flycheck-display-error-at-point "display")
  ("y" flycheck-copy-errors-as-kill "copy")

  ("s" flycheck-verify-setup "verify")
  ("t" flycheck-mode "toggle"))


;;
;; General (leader keys)
;; ref: https://github.com/noctuid/general.el/
;;
(use-package general
  :config
  (general-evil-setup)

  ;;
  ;; Frequent tasks
  ;;
  (global-set-key (kbd "<f2>") 'save-buffer)
  (global-set-key (kbd "<f3>") 'counsel-find-file)
  (global-set-key (kbd "<S-f3>") 'projectile-find-file)
  (global-set-key (kbd "<f8>") 'persp-switch-to-buffer)
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

  (general-nmap
    :keymaps 'override
    :state 'normal
    "M-q" 'evil-quit
    "M-o" 'delete-other-windows)

  ;;
  ;; Main
  ;;
  (general-nmap
    :keymaps 'override
    :state 'normal

    "SPC" 'hydra-main/body)

  ;;
  ;; dired
  ;;
  (general-define-key
    :keymaps 'dired-mode-map

    [f5] 'revert-buffer
    "TAB" 'dired-subtree-toggle)

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
   :keymaps 'override
   "\\" 'swiper)

  ;; expand-region
  (general-define-key
   :states 'visual
   :keymaps 'override
   "+" 'er/expand-region
   "-" 'er/contract-region)

  ;; paredit
  (general-define-key
   :state 'normal
   :keymaps 'paredit-mode-map

   ">" 'paredit-forward-slurp-sexp
   "<" 'paredit-forward-barf-sexp))


(provide 'core-keybindings)
