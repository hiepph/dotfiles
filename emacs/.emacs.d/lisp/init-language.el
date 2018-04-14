(require 'init-elpa)

(require-package 'go-mode)
(require-package 'clojure-mode)
(require-package 'yaml-mode)
(require-package 'rust-mode)
(require-package 'ess)


;; Go
;; format before saving
(add-hook 'before-save-hook 'gofmt-before-save)


(provide 'init-language)
