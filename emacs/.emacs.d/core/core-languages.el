;; Python
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)


;; Go
(use-package go-mode
  :config (progn
            ;; format before saving
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; import before saving
            (setq gofmt-command "goimports")))


;; Haskell
(use-package haskell-mode)

(defun haskell-evil-open-above ()
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))


;; Yaml
(use-package yaml-mode)


;; Org-mode
;; (use-package org
;;   :init
;;   (setq org-log-done 'time)
;;   (add-hook 'org-mode-hook 'turn-on-font-lock)
;;   ;; show image in org babel
;;   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;;   ;; Turn off auto-fold
;;   (setq org-startup-folded nil)

;;   :config
;;   (setq org-confirm-babel-evaluate nil
;;         org-src-fontify-natively t
;;         org-src-tab-acts-natively t)

;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '(
;;      (shell      . t)
;;      (emacs-lisp . t)
;;      (python     . t)
;;      (ruby       . t)
;;      ))
;;   )

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;   )

;; (use-package ob-async)


;;
;; Custom
;;
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))


(provide 'core-languages)
