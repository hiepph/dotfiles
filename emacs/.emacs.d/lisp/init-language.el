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
(setq web-mode-enable-auto-closing t)

(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(require-package 'js2-mode)


(provide 'init-language)
