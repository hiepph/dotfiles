;;; init.el --- Control all config iles

;;; Commentary:
;;;
;;; @hiepph's Emacs
;;; Philosophy:
;;; - Text is a universal interface

(package-initialize)

;;; Code:

;; Set custom variables file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Custom scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
;;; init.el ends here
