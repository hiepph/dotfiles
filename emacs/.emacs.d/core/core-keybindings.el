;; Principles:
;; 1. Mnemonic: c(ompile), b(uffers)
;; 2. Only high frequency tasks are bind to F keys
;; 3. Brain is for idea, not for storage. Keep yourself a reference!

;; References
;; 1. Spacemacs: https://develop.spacemacs.org/doc/DOCUMENTATION.html

;;
;; Menus
;;
(use-package hydra)

(defhydra hydra-main (:columns 4 :exit t)
  "Main"
  ;; Commands
  ("x" #'counsel-M-x "M-x")

  ;; buffers
  ("b" #'hydra-buffers/body "buffers")

  ;; workspaces
  ("w" #'hydra-workspace/body "workspaces")

  ;; dired
  ("d" #'dired-jump "dired")

  ;; Magit
  ("g" #'magit-status "magit")

  ;; Projectile
  ("p" #'hydra-projectile/body "projectile")

  ;; compile
  ("c" #'hydra-compile/body "compile")
  ("e" #'hydra-error/body "flycheck")

  ;; quick search
  ("/" #'counsel-rg "ripgrep")

  ;; registers
  ("r" #'counsel-evil-registers "registers")
  ("k" #'counsel-yank-pop "kill rings")

  ;; marks
  ("m" #'counsel-evil-marks "marks")

  ;; format
  ("t" #'hydra-toggle/body "toggle"))

(defhydra hydra-projectile (:columns 4 :exit t)
  "Projectile"
  ("p" #'projectile-switch-project "switch")
  ("f" #'projectile-find-file "find file")
  ("/" #'projectile-ripgrep  "ripgrep")
  ("d" #'projectile-find-dir "find directory")
  ("b" #'projectile-switch-to-buffer "buffer")
  ("k" #'projectile-kill-buffers "kill buffers")
  ("R" #'projectile-regenerate-tags "regenerate tags")
  ("j" #'projectile-find-tag "find tags")
  ("c" #'projectile-compile-project "compile (root)")
  ("!" #'projectile-run-shell-command-in-root "shell cmd (root)")
  ("&" #'projectile-run-async-shell-command-in-root "async cmd (root)")
  ("q" nil "Cancel" :color blue))

(defhydra hydra-buffers (:columns 4 :exit t)
  "Buffers"
  ("b" #'counsel-switch-buffer "buffer/rerecentf")
  ("k" #'~kill-current-buffer "kill")
  ("d" #'magit-diff-buffer-file "git diff"))

(defhydra hydra-compile (:columns 4 :exit t)
  "Compile"
  ("c" #'compile "compile")
  ("r" #'~recompile "recompile")
  ("k" #'kill-compilation "kill"))

(defhydra hydra-error (:columns 4 :exit t)
  "Error handling "
  ("l" #'flycheck-list-errors "list")
  ("n" #'flycheck-next-error "next")
  ("p" #'flycheck-previous-error "previous")

  ("!" #'flycheck-display-error-at-point "display")
  ("y" #'flycheck-copy-errors-as-kill "copy")

  ("s" #'flycheck-verify-setup "verify")
  ("t" #'flycheck-mode "toggle"))

(defhydra hydra-workspace (:columns 4 :exit t)
  "Workspaces"
  ("c" #'eyebrowse-create-window-config "create")
  ("n" #'eyebrowse-next-window-config "next")
  ("p" #'eyebrowse-previous-window-config "previous")
  ("x" #'eyebrowse-close-window-config "close")
  ("'" #'eyebrowse-last-window-config "last")
  ("," #'eyebrowse-rename-window-config "rename"))

(defhydra hydra-toggle (:columns 4 :exit t)
  ("f" #'format-mode "format") ;; off by default
  ("c" #'company-mode "company") ;; on by default
  )

;;
;; general (leader keys)
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
  (global-set-key (kbd "<f8>") 'counsel-buffer-or-recentf)
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

  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)



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
  ;; company
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next-or-abort
   "C-p" 'company-select-previous-or-abort
   ;; "C-/" 'company-complete
   )

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
