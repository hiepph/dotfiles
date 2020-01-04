;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Set custom variables file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Custom scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; start emacs server
;; easy to open files in existing emacs windows: emacsclient -n file1 file2 ...
;; (server-start)

(require 'init-elpa)
(require 'init-language)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-helper)
(require 'init-miscellaneous)
(require 'init-keys)
(require 'init-custom nil 'no-error)

(provide 'init)
