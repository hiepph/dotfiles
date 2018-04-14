(require 'init-elpa)


;; Helm
(require-package 'helm)
(helm-mode 1)

;; Set Helm to use `ripgrep`
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

;; Replace default Emacs utilities with Helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Always stay as separated below window
(setq helm-always-two-windows t)
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)

;; Fuzzy match
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Auto resize to fit the number of candidates
(helm-autoresize-mode t)

;; Show kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


(provide 'init-search)
