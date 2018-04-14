(require 'init-elpa)

(require-package 'clojure-mode)
(require-package 'yaml-mode)
(require-package 'ess)


;; Go
(require-package 'go-mode)

;; format before saving
(add-hook 'before-save-hook 'gofmt-before-save)


;; Rust
(require-package 'rust-mode)
(require-package 'flycheck-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(provide 'init-language)
