(require 'init-elpa)

;; Fonts
(set-frame-font "Fira Code 14")
(set-face-attribute 'default nil :height 125)

;; Theme
(load-theme 'doom-city-lights t)

;; Python flycheck
(setq flycheck-python-pylint-executable "~/miniconda3/bin/pylint")
(setq flycheck-python-flake8-executable "~/miniconda3/bin/flake8")

(provide 'init-custom)
