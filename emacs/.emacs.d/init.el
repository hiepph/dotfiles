;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Custom scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-language)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-mode)
(require 'init-search)
(require 'init-helper)
(require 'init-miscellaneous)
(require 'init-custom nil 'no-error)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e" "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "503385a618581dacd495907738719565243ab3e6f62fec8814bade68ef66e996" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" default)))
 '(package-selected-packages
   (quote
    (ess melancholy-theme ample-theme color-theme-sanityinc-tomorrow popup-pos-tip pos-tip rust-mode clojure-mode expand-region go-mode edit-indirect auto-complete fzf git-gutter-fringe+ git-gutter+ magit sublime-themes smex ripgrep rainbow-delimiters projectile neotree markdown-mode ido-ubiquitous golden-ratio git-gutter dracula-theme autopair atom-one-dark-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
