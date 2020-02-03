;; init.el --- Control all config iles

;;
;; @hiepph's Emacs
;;

(package-initialize)


;; Set custom variables file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Custom scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-miscellaneous)
(require 'init-ui)
(require 'init-language)
(require 'init-edit)
(require 'init-helper)
(require 'init-keys)
(require 'init-custom nil 'no-error)

(provide 'init)
