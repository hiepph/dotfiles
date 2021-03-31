;; Principles:
;; 1. Mnemonic: c(ompile), b(uffers)
;; 2. Only high frequency tasks are bind to F keys
;; 3. Brain is for idea, not for storage. Keep yourself a reference!
;; 4. Reduce complex key sequences to a function

;; References
;; 1. Spacemacs: https://develop.spacemacs.org/doc/DOCUMENTATION.html

;;
;; Menus
;;
(use-package hydra)

(defhydra hydra-main (:columns 4 :exit t)
  "Main"
  ;; Commands
  ("x" #'execute-extended-command "M-x")

  ;; buffers
  ("b" #'hydra-buffers/body "buffers")

  ;; workspaces
  ("w" #'hydra-workspace/body "workspaces")

  ;; dired
  ("d" #'dired-jump "dired")

  ;; Magit
  ("g" #'magit-status "git status")
  ("G" #'hydra-magit/body "magit")

  ;; Projectile
  ("p" #'hydra-projectile/body "projectile")

  ;; compile
  ("c" #'hydra-compile/body "compile")
  ("e" #'hydra-error/body "flycheck")

  ;; consult
  ("r" #'~consult-register "registers")
  ("m" #'~consult-mark "marks")

  ;; kill-ring
  ("y" #'consult-yank-pop "yank pop")

  ;; ripgrep
  ("/" #'consult-ripgrep "ripgrep")

  ;; dumb-jump
  ("j" #'hydra-dumb-jump/body "jump")

  ;; format
  ("t" #'hydra-toggle/body "toggle"))

(defhydra hydra-projectile (:columns 4 :exit t)
  "Projectile"
  ("p" #'projectile-switch-project "switch")

  ("f" #'projectile-find-file "find file")

  ("/" #'~projectile-ripgrep  "ripgrep")

  ("d" #'projectile-find-dir "find directory")
  ("D" #'projectile-dired-other-window "dired")

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
  ("f" #'find-file "find")
  ("b" #'consult-buffer "buffers")
  ("B" #'consult-buffer-other-window "buffers (other)")
  ("r" #'~list-recent-files "recent")
  ("k" #'~kill-current-buffer "kill current")
  ("K" #'~kill-all-buffers "kill all"))

(defhydra hydra-compile (:columns 4 :exit t)
  "Compile"
  ("c" #'~compile "compile")
  ("C" #'~compile-current-file "compile current")
  ("r" #'~recompile "recompile")

  ("e" #'consult-compile-error "error")

  ("t" #'~test-all-files "test all")
  ("T" #'~test-current-file "test current")

  ("k" #'kill-compilation "kill")

  ("!" #'shell-command "cmd")
  ("&" #'async-shell-command "async cmd"))

(defhydra hydra-error (:columns 4 :exit t)
  "Error handling "
  ("c" #'consult-flycheck "consult")

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
  ("p" #'eyebrowse-prev-window-config "previous")
  ("x" #'eyebrowse-close-window-config "close")
  ("." #'eyebrowse-switch-to-window-config "switch")
  ("'" #'eyebrowse-last-window-config "last")
  ("," #'eyebrowse-rename-window-config "rename"))

(defhydra hydra-toggle (:columns 4 :exit t)
  ("f" #'format-mode "format") ;; off by default
  ("c" #'company-mode "company") ;; on by default
  ("s" #'flyspell-mode "flyspell") ;; on by default in text-mode
  ("i" #'indent-guide-mode "indent")
  ("p" #'paredit-mode "paredit") ;; on by default in some predefined extensions
  )

(defhydra hydra-dumb-jump (:color blue :columns 4)
  "dumb jump"
  ("j" #'dumb-jump-go "go")
  ("o" #'dumb-jump-go-other-window "other window")
  ("e" #'dumb-jump-go-prefer-external "go external")
  ("x" #'dumb-jump-go-prefer-external-other-window "go external other window")
  ("i" #'dumb-jump-go-prompt "prompt")
  ("l" #'dumb-jump-quick-look "quick look")
  ("b" #'dumb-jump-back "back"))

(defhydra hydra-magit (:color blue :columns 4)
  "Magit"
  ("?" #'magit-dispatch "help")
  ("l" #'magit-log-buffer-file "log history")
  ("d" #'magit-diff-buffer-file "diff"))

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
  (global-set-key (kbd "<f3>") 'find-file)
  (global-set-key (kbd "S-<f3>") 'find-file-other-window)
  (global-set-key (kbd "<f8>") 'ido-switch-buffer)
  (global-set-key (kbd "<f9>") '~compile-current-file)
  (global-set-key (kbd "<f12>") '~test-current-file)
  (global-set-key (kbd "S-<f12>") '~test-all-files)

  (global-set-key (kbd "<escape>") 'evil-normal-state)

  ;;
  ;; Alt combination
  ;;
  (global-set-key (kbd "M-<return>") 'wand:execute)
  (global-set-key (kbd "M-&") 'async-shell-command)
  (global-set-key (kbd "M-|") '~acme|)
  (global-set-key (kbd "M-$") '~acme$)
  (global-set-key (kbd "M-<") '~acme<)

  (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (global-set-key (kbd "M-'") 'eyebrowse-last-window-config)


  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)


  ;;
  ;; Main
  ;;
  (general-nmap
    :keymaps 'override
    :state '(normal emacs)

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
  ;; Dired
  ;;
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map

   "yy" 'dired-ranger-copy
   "pp" 'dired-ranger-paste
   "pP" 'dired-ranger-move)

  ;;
  ;; Editor
  ;;
  ;; company
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next-or-abort
   "C-p" 'company-select-previous-or-abort)

  (general-define-key
   :keymaps 'company-mode-map
   "C-/" 'company-complete)

  ;; tags
  (general-define-key
   :states 'normal
   :keymaps 'xref--xref-buffer-mode-map
   "q" 'quit-window
   "RET" 'xref-goto-xref)

  ;; expand-region
  (general-define-key
   :states 'visual
   :keymaps 'override
   "+" 'er/expand-region
   "-" 'er/contract-region)

  ;; Undo/Redo
  (general-define-key
   :states 'normal
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo)

  ;; dumb-jump
  (general-define-key
   :states 'normal
   "C-]" 'dumb-jump-go)
  )


(provide 'core-keybindings)
