;; use use-package as package bootstraping
(straight-use-package 'use-package)
;; auto specifying `:straight t` to automatically install missing packages
(setq straight-use-package-by-default t)


;;
;; Garbage collector hacks
;;
(use-package gcmh
  :straight (gcmh :type git :host github :repo "emacsmirror/gcmh")
  :init
  (gcmh-mode 1))


(provide 'core-packages)
