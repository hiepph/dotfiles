(require 'init-elpa)

(let ((languages '(go-mode
                   yaml-mode
                   rust-mode
                   js2-mode)))
  (dolist (lang languages) (unless (package-installed-p lang)
                             (package-install lang))))

;; Python
;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)


;; Go
(use-package go-mode
  :ensure t
  :config (progn
            ;; format before saving
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; import before saving
            (setq gofmt-command "goimports")))


;; Rust
(use-package flycheck-rust
  :ensure t
  :config (with-eval-after-load 'rust-mode
            (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


;; Lisp
(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

;; Javascript & Web-related
(use-package web-mode
  :ensure t
  :init (progn
          (setq web-mode-enable-auto-closing t)
          (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))))


;; Org-mode
(use-package org
  :ensure t
  :init (progn
          (setq org-log-done 'time)

          ;; Turn off auto-fold
          (setq org-startup-folded nil)))
(add-hook 'org-mode-hook 'turn-on-font-lock)


(provide 'init-language)
