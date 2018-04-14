(require 'init-elpa)


;; Helm
(require-package 'helm)
(helm-mode 1)

;; Set Helm to use `ripgrep`
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

;; Replace tab-completion
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; Always stay as separated below window
(setq helm-always-two-windows t)
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)


(provide 'init-search)
