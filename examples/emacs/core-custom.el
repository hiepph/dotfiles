;; theme
(use-package plan9-theme)
(load-theme 'plan9 t)

;; font
(set-frame-font "Input Mono" nil t)
(add-to-list 'default-frame-alist
             '(font . "Input Mono"))

(provide 'core-custom)
