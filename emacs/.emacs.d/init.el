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
 '(package-selected-packages
   (quote
<<<<<<< HEAD
	(yaml-mode clojure-mode expand-region go-mode edit-indirect auto-complete fzf git-gutter-fringe+ git-gutter+ magit sublime-themes smex ripgrep rainbow-delimiters projectile neotree markdown-mode ido-ubiquitous golden-ratio git-gutter dracula-theme autopair atom-one-dark-theme all-the-icons))))
=======
	(rust-mode clojure-mode expand-region go-mode edit-indirect auto-complete fzf git-gutter-fringe+ git-gutter+ magit sublime-themes smex ripgrep rainbow-delimiters projectile neotree markdown-mode ido-ubiquitous golden-ratio git-gutter dracula-theme autopair atom-one-dark-theme all-the-icons))))
>>>>>>> f7c6b17d5963da83f6072f3dbd3916a5a381c4dd
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
