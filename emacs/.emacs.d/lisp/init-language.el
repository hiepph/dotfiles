(require 'init-elpa)

(require-package 'clojure-mode)
(require-package 'yaml-mode)
;; (require-package 'ess)


;; Go
(require-package 'go-mode)

;; format before saving
(add-hook 'before-save-hook 'gofmt-before-save)
;; import before saving
(setq gofmt-command "goimports")


;; Rust
(require-package 'rust-mode)
(require-package 'flycheck-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; Scala
;; (require-package 'ensime)


;; Lisp
(require-package 'slime)
(setq inferior-lisp-program "sbcl")

;; Javascript & Web-related
(require-package 'web-mode)
(require-package 'js2-mode)
(require-package 'vue-mode)
(require-package 'vue-html-mode)
(setq web-mode-enable-auto-closing t)


(provide 'init-language)
