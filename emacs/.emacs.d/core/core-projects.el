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

;; (use-package consult-selectrum
;;   :demand t)



;;
;; Registers
;;

;; some special registers:
;; % current file path
;; : recently executed command
;; / last search pattern
;; + clipboard
(defun ~consult--register-candidates (register-list)
  (seq-filter
   (lambda (item) (not (null (cadr item))))
   (mapcar
    (lambda (reg)
      (let ((v (evil-get-register reg t)))
        (list
         (cond ((eq reg ?%) (format "%c (filepath) -- %s" reg v))
               ((eq reg ?:) (format "%c (command) -- %s" reg v))
               ((eq reg ?/) (format "%c (search) -- %s" reg v))
               ((eq reg ?+) (format "%c (clipboard) -- %s" reg v))
               (t (format "%c -- %s" reg v)))
         v)))
    register-list)))

(defun ~consult-register ()
  "View the list of evil registers with corresponding text.
Number registers are not needed because it is easier to refer from the `yank-pop`."
  (interactive)
  (let* ((selectrum-should-sort nil)
         (chars (~consult--register-candidates (cl-loop for c from ?a to ?z collect c)))
         (special (~consult--register-candidates '(?% ?: ?/ ?+)))
         (candidate (completing-read "Registers: " (append chars special))))
    (insert (cadr (assoc candidate chars)))))


;;
;; Marks
;;
;; some special marks:
;; % matching parentheses
;; (/)   prev/next sentence
;; {/}   prev/next paragrapc
;; H/M/L top/middle/bottom
;; gf    filename under cursor
;; </>   prev/next visual selection
;;


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
                   (format "rg -i --line-number --hidden -S -g '!.git' '%s' %s"
                           q dir)))))
           (candidate (s-split ":" (completing-read
                                    (format "[%s]: " q)
                                    res)))
           (file-name (car candidate))
           (jump-point (string-to-number (cadr candidate))))
      (find-file (expand-file-name file-name dir))
      (goto-line jump-point)))

(defun ~ripgrep (q)
  "Search current directory"
  (interactive "Mrg: ")
  (~ripgrep-search q default-directory nil))

(defun ~projectile-ripgrep (q)
  "Search in project root"
  (interactive
   (list (read-string (format "[%s]-rg: " (projectile-project-name)))))
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
