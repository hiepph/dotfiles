(require 'init-elpa)

(require-package 'go-mode)

;; format before saving
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-language)
