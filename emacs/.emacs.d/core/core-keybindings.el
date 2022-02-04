;;
;; Key hints
;; ref: https://github.com/justbur/emacs-which-key
;;
(use-package which-key
  :init
  (which-key-mode))

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
  ("`" #'~consult-mark "marks")

  ;; kill-ring
  ("y" #'consult-yank-pop "yank")

  ("/" #'~ripgrep "ripgrep")

  ;; dumb-jump
  ("j" #'hydra-dumb-jump/body "jump")

  ("m" #'hydra-mode/body "toggle"))

(defhydra hydra-projectile (:columns 4 :exit t)
  "Projectile"
  ("p" #'projectile-switch-project "switch")

  ("f" #'projectile-find-file "find file")

  ("/" #'~projectile-ripgrep  "ripgrep")
  ("g" #'projectile-grep  "grep")

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

;;
;; Some operations that relates to the workspace:
;; + add: query and add buffer to the current workspace
;; + set: add the buffer and remote from the others
;; + kill current: remove the current buffer
;; + kill: query and remove
;;
(defhydra hydra-buffers (:columns 4 :exit t)
  "Buffers"
  ("f" #'find-file "find")
  ("b" #'persp-switch-to-buffer* "list")
  ("r" #'~list-recent-files "recent")
  ("a" #'persp-add-buffer "add")
  ("s" #'persp-set-buffer "set")
  ("k" #'~persp-remove-current-buffer "kill current")
  ("K" #'persp-remove-buffer "kill"))

(defhydra hydra-compile (:columns 4 :exit t)
  "Compile"
  ("c" #'~compile "compile")
  ("r" #'~recompile "recompile")

  ("e" #'consult-compile-error "error")

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
  ("w" #'persp-switch "workspace")
  ("n" #'persp-next "next")
  ("p" #'persp-prev "previous")
  ("x" #'~persp-kill-current "close current")
  ("X" #'persp-kill "close")
  ("'" #'persp-switch-last "switch to last")
  ("s" #'persp-state-save "state save")
  ("l" #'persp-state-load "state save")
  ("," #'persp-rename "rename"))

(defhydra hydra-mode (:columns 4 :exit t)
  "All off by default"
  ("c" #'company-mode "company")
  ("C" #'global-company-mode "company (global)")
  ("e" #'flycheck-mode "flycheck")
  ("E" #'global-flycheck-mode "flycheck (global)")
  ("f" #'format-mode "format")
  ("F" #'format-global-mode "format (global)")
  ("s" #'flyspell-mode "flyspell")
  ("d" #'direnv-mode "direnv")
  ("i" #'indent-guide-mode "indent")
  ("I" #'indent-guide-global-mode "indent (global)")
  ("\\" #'fci-mode "column")
  ("|" #'fci-global-mode "column (global)"))

(defhydra hydra-dumb-jump (:color blue :columns 4)
  "dumb jump"
  ("j" #'dumb-jump-go "go")
  ("o" #'dumb-jump-go-other-window "other window")
  ("i" #'dumb-jump-go-prompt "prompt")
  ("l" #'dumb-jump-quick-look "quick look")
  ("b" #'dumb-jump-back "back"))

(defhydra hydra-magit (:color blue :columns 4)
  "Magit"
  ("?" #'magit-dispatch "help")
  ("l" #'magit-log-buffer-file "log history")
  ("d" #'magit-diff-buffer-file "diff"))

(defhydra hydra-org (:color blue :columns 4)
  "Org"
  ("h" #'org-insert-heading "heading")
  ("H" #'org-insert-subheading "sub heading")
  ("l" #'org-insert-link "link")
  ("," #'org-insert-structure-template "template")
  ("c" #'org-ctrl-c-ctrl-c "execute")
  ("'" #'org-edit-special "edit"))

(defhydra hydra-org-toggle (:color blue :columns 4)
  ("h" #'org-toggle-heading "heading")
  ("l" #'org-toggle-item "list"))

(defhydra hydra-cider (:color blue :columns 4)
  "Cider"
  ("j" #'cider-jack-in "jack in")
  ("d" #'cider-clojuredocs "docs")
  ("b" #'cider-debug-defun-at-point "debug")
  ("c" #'cider-eval-defun-at-point "eval at point")
  ("C" #'cider-eval-defun-up-to-point "eval up to point")
  ("x" #'cider-interrupt "interrupt")
  ("f" #'cider-eval-file "eval file")
  ("t" #'cider-toggle-trace-var "trace var")
  ("i i" #'cider-inspect-defun-at-point "inspect")
  ("i n" #'cider-inspector-next-page "inspect next page")
  ("i p" #'cider-inspector-prev-page "inspect prev page")
  ("p" #'cider-pprint-eval-defun-at-point "print")
  ("P" #'cider-pprint-eval-defun-to-comment "print to comment")
  ("q" #'cider-quit "quit"))

;;
;; general (leader keys)
;; ref: https://github.com/noctuid/general.el/
;;
(use-package general
  :config
  ;;
  ;; Evil
  ;;
  (general-evil-setup)

  ;; escape to normal-state even when in ~emacs-state~
  (global-set-key (kbd "<escape>") 'evil-normal-state)

  ;;
  ;; Frequent tasks
  ;;
  (global-set-key (kbd "<f1>") 'persp-switch)
  (global-set-key (kbd "<f2>") 'save-buffer)
  (global-set-key (kbd "<f3>") 'find-file)
  (global-set-key (kbd "<f4>") 'persp-switch-to-buffer*)

  ;;
  ;; Alt combination
  ;;

  ;; M-: eval-expression
  (global-set-key (kbd "M-&") 'async-shell-command)

  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)

  (global-set-key (kbd "<M-left>") 'windmove-left)
  (global-set-key (kbd "<M-right>") 'windmove-right)
  (global-set-key (kbd "<M-down>") 'windmove-down)
  (global-set-key (kbd "<M-up>") 'windmove-up)

  ;;
  ;; Main
  ;;
  (general-nmap
    :keymaps 'override
    :state '(normal emacs)
    "SPC" 'hydra-main/body)

  ;;
  ;; Org mode
  ;; ref:
  ;;  + evil-org-mode: https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  ;;  + reference card: https://orgmode.org/worg/orgcard.html
  ;;
  ;; Work out-of-the-box:
  ;; +) ]]: next same-level element
  ;; +) [[: previous same-level element
  ;;
  (general-nmap
   :keymaps 'org-mode-map
   :states '(visual normal)
   "," 'hydra-org/body
   "> >" 'org-shiftmetaright
   "< <" 'org-shiftmetaleft
   "g u" 'outline-up-heading)

  (general-nmap
    :states 'insert
    :keymaps 'org-mode-map
    "RET" 'org-newline-and-indent
    "M-RET" 'org-meta-return)

  ;;
  ;; Cider (Clojure)
  ;;
  (general-nmap
    :keymaps '(clojure-mode-map cider-repl-mode-map)
    :states 'normal
    "," 'hydra-cider/body)

  ;;
  ;; Emacs Lisp
  ;;
  (general-define-key
   :states 'normal
   :keymaps 'emacs-lisp-mode-map
   [f5] '~eval-buffer)

  ;;
  ;; company
  ;;
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next-or-abort
   "C-p" 'company-select-previous-or-abort)

  (general-define-key
   :keymaps 'company-mode-map
   "C-/" 'company-complete)

  ;;
  ;; tags
  ;;
  (general-define-key
   :states 'normal
   :keymaps 'xref--xref-buffer-mode-map
   "q" 'quit-window
   "RET" 'xref-goto-xref)

  ;;
  ;; expand-region
  ;;
  (general-define-key
   :states 'visual
   :keymaps 'override
   "+" 'er/expand-region
   "-" 'er/contract-region
   "d" 'er/mark-defun
   "s" 'er/mark-symbol
   "u" 'er/mark-url
   "c" 'er/mark-comment)

  ;;
  ;; Undo/Redo
  ;;
  (general-define-key
   :states 'normal
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo)

  ;;
  ;; Tabs (Centaur)
  ;;
  (general-define-key
   :states 'normal
   :keymaps 'centaur-tabs-mode-map
   "g t" 'centaur-tabs-forward
   "g T" 'centaur-tabs-backward)

  ;;
  ;; dumb-jump
  ;;
  (general-define-key
   :states 'normal
   ;; (?) 'dumb-jump-back
   "C-]" 'dumb-jump-go))


(provide 'core-keybindings)
