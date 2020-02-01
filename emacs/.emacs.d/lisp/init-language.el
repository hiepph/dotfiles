;;; init-language.el --- Programming languages and modes

(require 'init-elpa)


;; Python
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
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


;; Elixir
(use-package elixir-mode
  :ensure t)


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


;; Haskell
(use-package haskell-mode
  :ensure t)


;; Org-mode
(use-package org
  :ensure t
  :init
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  ;; show image in org babel
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Turn off auto-fold
  (setq org-startup-folded nil)

  :config
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell      . t)
     (emacs-lisp . t)
     (python     . t)
     (ruby       . t)
     ))
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )


;;
;; Custom
;;
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))



(provide 'init-language)
