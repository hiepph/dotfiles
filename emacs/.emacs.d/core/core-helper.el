(use-package s)

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
         (all-candidate (append chars special))
         (candidate (completing-read "Registers: " all-candidate)))
    (insert (cadr (assoc candidate all-candidate)))))



;;
;; Marks
;; ref:
;;   https://vim.fandom.com/wiki/Using_marks
;;
;; some special marks:
;;
;; .     last change occurs
;; ^     last insert
;; '     last line
;; `     last position
;; %     matching parentheses
;; (/)   prev/next sentence
;; {/}   prev/next paragraph
;; H/M/L top/middle/bottom
;; gf    filename under cursor
;; </>   prev/next visual selection
;;
(defun ~consult--mark-candidates (register-list)
  (seq-filter
   (lambda (item) (not (null (cadr item))))
   (mapcar
    (lambda (mark)
      (let ((pos (evil-get-marker mark t)))
        (if pos
            (let ((buffer (marker-buffer pos)))
              (with-current-buffer buffer
                (save-excursion
                  (goto-char pos)
                  (let ((line-content (thing-at-point 'line)))
                    (list (format "%c (%s:%d:%d) -- %s"
                                  mark
                                  (buffer-name buffer)
                                  (line-number-at-pos)
                                  (current-column)
                                  line-content) pos))))))))
    register-list)))

(defun ~consult-mark ()
  (interactive)
  (let* ((selectrum-should-sort nil)
         (lower-chars (~consult--mark-candidates
                         (cl-loop for c from ?a to ?z collect c)))
         (upper-chars (~consult--mark-candidates
                         (cl-loop for c from ?A to ?Z collect c)))
         (all-candidate (append lower-chars upper-chars))
         (candidate (completing-read "Marks: " all-candidate)))
    (goto-char (cadr (assoc candidate all-candidate)))))



;;
;; Scroll
;;
;; z -
;; t: scroll to top
;; z: scroll to middle
;; b: scroll to bottom
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


;;
;; Compile
;;
;; show stack trace on error
;; (setq debug-on-error t)

(defun ~compile (command)
  (interactive "M~compile: ")
  (compile (s-replace "%" (evil-get-register ?% t) command)))

(defun ~run-current-file (f command-map)
  "Run command map with function f
f can be: compile, ~acme$, ~acme&, ~acme!"
  (interactive)
  (save-buffer)

  (let* ((fname (s-chop-suffix (car (s-match "<.*>" (buffer-name))) (buffer-name)))
         (suffix (file-name-extension fname))
         (prog (cdr (assoc suffix command-map))))
    (if (null prog)
        (error "Extension is not yet registered")
      (funcall f (format "%s %s" prog (shell-quote-argument fname))))))

(defvar *compile-command-map* '(("py" . "python")
                                ("go" . "go run")
                                ("rb" . "ruby")
                                ("hs" . "runhaskell")
                                ("sh" . "bash")))

(defun ~compile-current-file ()
  "(re)compile the current file. A replacement for compile with automatic filetype recognition.
e.g. If the current buffer is hello.py, then it'll call python hello.py
"
  (interactive)
  (save-buffer)
  (~run-current-file 'compile *compile-command-map*))

;; default compile command to empty
(setq compile-command "")

(defun ~recompile ()
  "custom recompile "
  (interactive)
  (save-buffer)
  (recompile))

(defvar *test-command-map* '(("py" . "pytest -s -v")
                             ("go" . "go test")))

(defun ~test-current-file ()
  "Test current file using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest hello.py
"
  (interactive)
  (~run-current-file 'compile *test-command-map*))

(defun ~test-all-files ()
  "Test all files in same directory using 'compile'. Automatic filetype recogntion.
e.g. If the current buffer is hello.py, then it'll call pytest
"
  (interactive)
  (save-buffer)

  (let* ((fname (buffer-name))
         (suffix (file-name-extension fname))
         (prog (cdr (assoc suffix *test-command-map*)))
         (command prog))
    (if (null prog)
        (error "Compile command not found. Please check '*<?>-command-map*'")
      (compile command))))


;;
;; ACME
;; REF: http://man.cat-v.org/plan_9/1/acme
;;
(defun ~acme$ (&optional command)
  "spawn a terminal and execute command

eg:
$ ls
"
  (interactive "M$: ")
  (let ((open-term "alacritty -e $SHELL -c"))
    (call-process-shell-command
     (format "%s '%s; $SHELL -i'" open-term command)
     nil)))


(defun ~acme< (&optional command)
  "Execute command and pipe stdout to editor

eg:
< ls
"
  (interactive "M<: ")
  (delete-region (region-beginning)
                 (region-end))
  (insert (s-trim-right (shell-command-to-string command))))


(defun ~acme| (&optional command)
  "Pipe input into command line and output stdout to editor"
  (interactive "M|: ")
  (let ((inhibit-message t))
    (shell-command-on-region (region-beginning)
                             (region-end)
                             command
                             ;; output
                             (current-buffer)
                             ;; replace?
                             t
                             ;; name of error buffer (nil = current-buffer)
                             nil
                             ;; show error buffer
                             0)))


;;
;; Buffers
;;
(defun ~eval-buffer ()
  "eval-buffer but with message"
  (interactive)
  (eval-buffer)
  (message "> Eval buffer succeeded"))


(defun ~kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ~kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'core-helper)
