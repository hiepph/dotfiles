(require 'init-elpa)


;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))


;; Power-up by helm
(use-package helm-projectile
  :diminish helm-mode
  :ensure t
  :config (progn
            (setq projectile-completion-system 'helm)
            (helm-projectile-on)))

;; Helm
(use-package helm
  :ensure t
  ;; :diminish helm-mode
  :init (progn
          ;; Set Helm to use `ripgrep`
          (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
          (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
          ;; Always stay as separated below window
          (setq helm-always-two-windows t)
          (setq helm-split-window-in-side-p t)
          (setq helm-split-window-default-side 'below)

          ;; Fuzzy match
          (setq helm-M-x-fuzzy-match t)
          (setq helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t))
  :config (progn
            (helm-mode 1)
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
            ;; Auto resize to fit the number of candidates
            (helm-autoresize-mode t))
  :bind (
         ;; Replace default Emacs utilities with Helm
         ("M-x" . 'helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x b" . 'helm-mini)
         ("C-x C-f" . 'helm-find-files)
         ;; Show kill ring
         ("M-y" . 'helm-show-kill-ring)))

;; Recent files
(use-package recentf
  :config (progn
            (setq recentf-save-file (concat user-emacs-directory ".recentf"))
            (recentf-mode 1)
            (setq recentf-max-menu-items 40))
  :bind ("C-x C-r" . 'recentf-open-files))


;; IDO
(use-package ido
  :diminish ido-mode
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (ido-ubiquitous-mode 1)
            (setq ido-enable-flex-matching t)
            (setq ido-use-filename-at-point nil)
            (setq ido-auto-merge-work-directories-length -1)
            (setq ido-use-virtual-buffers t)
            ;; Shows a list of buffers
            (global-set-key (kbd "C-x C-b") 'ibuffer)))

(use-package ido-completing-read+)


;; Enable move point from window to window using Shift and the arrow keys
(windmove-default-keybindings)
;; and C-c + {h,j,k,l}
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Virtual desktop
(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))


(provide 'init-navigation)
