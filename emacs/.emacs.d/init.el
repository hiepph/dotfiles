;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Custom scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-mode)
(require 'init-search)
(require 'init-language)
(require 'init-helper)
(require 'init-miscellaneous)

; Lang support

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "503385a618581dacd495907738719565243ab3e6f62fec8814bade68ef66e996" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" default)))
 '(package-selected-packages
   (quote
    (ess melancholy-theme ample-theme color-theme-sanityinc-tomorrow popup-pos-tip pos-tip rust-mode clojure-mode expand-region go-mode edit-indirect auto-complete fzf git-gutter-fringe+ git-gutter+ magit sublime-themes smex ripgrep rainbow-delimiters projectile neotree markdown-mode ido-ubiquitous golden-ratio git-gutter dracula-theme autopair atom-one-dark-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
