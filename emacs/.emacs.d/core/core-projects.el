;;
;; dired
;; ref: https://github.com/Fuco1/dired-hacks
;;
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-ranger)
;; (use-package dired-list)

;; readable information
(setq dired-listing-switches "-alh")

;; auto revert dired-mode
(add-hook 'dired-mode-hook 'auto-revert-mode)


;;
;; Magit
;; ref: https://magit.vc/
;;
(use-package magit)


;;
;; Key hints
;; ref: https://github.com/justbur/emacs-which-key
;;
(use-package which-key
  :init
  (which-key-mode))


;;
;; Selectrum
;; ref: https://github.com/raxod502/selectrum
;; incremental narrowing search
;;
(use-package selectrum
  :init
  (selectrum-mode +1))


;;
;; Like swiper, selectrum engine
;; ref: https://github.com/raxod502/ctrlf
;;
(use-package ctrlf)


;;
;; Prescient
;; ref: https://github.com/raxod502/prescient.el
;; save command history on disk,
;; so the sorting gets more intelligent over time
;;
(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package company-prescient
  :init
  (company-prescient-mode +1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1))

(use-package consult-selectrum
  :demand t)

;;
;; Search helper
;;
(defun ~ripgrep-search (q dir projectile?)
  "Search using ripgrep and provide results through Selectrum"
    (unless (executable-find "rg")
        (user-error "'rg' not found"))
    (let* ((res (mapcar
                 (lambda (line) (car (last (s-split dir line))))
                 (s-split
                  "\n"
                  (shell-command-to-string
                   (format "rg -i --line-number --hidden -S -g '!.git' %s %s"
                           q dir)))))
           (candidate (s-split ":" (completing-read
                                    (format "[%s]: " q)
                                    res)))
           (file-name (car candidate))
           (jump-point (string-to-number (cadr candidate))))
      (find-file (expand-file-name file-name dir))
      (goto-line jump-point)))

;; TODO: at-point -> visual
;; (defun ~ripgrep-at-point (q)
;;   "Search current directory"
;;   (interactive
;;    (list (read-string "rg: " (thing-at-point 'symbol))))
;;   (~ripgrep-search q default-directory nil))

(defun ~ripgrep (q)
  "Search current directory"
  (interactive "Mrg: ")
  (~ripgrep-search q default-directory nil))

(defun ~projectile-ripgrep (q)
  "Search in project root"
  (interactive
   (list (read-string (format "%s-rg: " (projectile-project-name)))))
  (~ripgrep-search q (projectile-project-root) t))

;;
;; Desktops management
;;
(use-package eyebrowse
  :diminish
  :init
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))


;;
;; Recent files
;;
(use-package recentf
  :config
  (recentf-mode 1))

;; select recent files
(defun ~list-recent-files ()
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Recent files: " files nil t))))

;;
;; Project management
;;
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)

  ;; sort files by recently opened
  (setq projectile-sort-order 'recentf)

  ;; open top-level directory instead of a specific files
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )

;;
;; Jump to definition, even without CTAGS
;; ref: https://github.com/jacktasia/dumb-jump
;;
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(provide 'core-projects)
