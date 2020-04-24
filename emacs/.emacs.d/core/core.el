;; config package managers, should be put first
(require 'core-packages)

;; Languages
(require 'core-languages)
;; Editor
(require 'core-editor)
;; Projects management and support
(require 'core-projects)

;; Themes, looks, etc.
(require 'core-ui)

;; config all keybindings, should be put last
(require 'core-keybindings)

;; custom config for each
(require 'core-custom nil 'noerror)

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'core)
