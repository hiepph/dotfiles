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

(provide 'core)
