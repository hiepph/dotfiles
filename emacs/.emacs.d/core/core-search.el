;;
;; consult
;; ref: https://github.com/minad/consult
;;
(use-package consult
  :config
  (fset 'multi-occur #'consult-multi-occur)

  ;; :config
  ;; (consult-preview-mode)
  )

(use-package consult-flycheck)


;;
;; Marginalia adds annotations in the minibuffer
;; ref: https://github.com/minad/marginalia
;;
(use-package marginalia
  :config
  (marginalia-mode)
  (advice-add
   #'marginalia-cycle :after
   (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))


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
;; Embark
;; + generates candidates to insert into the buffer
;;
;; (use-package embark)


;;
;; Registers
;;
;; some special registers:
;; % current file path
;; : recently executed command
;; / last search pattern
;; + clipboard
;;
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
;; Interactive Search with the help of ripgrep
;;
(defun ~ripgrep-search (q dir projectile?)
  "Search using ripgrep and provide results through Selectrum"
    (unless (executable-find "rg")
        (user-error "'rg' not found."))
    (let* ((res (mapcar
                 (lambda (line) (car (last (s-split dir line))))
                 (s-split
                  "\n"
                  (shell-command-to-string
                   (format "rg -i --line-number --hidden -S -g '!.git' '%s' %s"
                           q dir)))))
           (candidate (if (= (length res) 1)
                          (user-error (format "'%s' not found." q))
                          (s-split ":" (completing-read
                                        (format "[%s]: " q)
                                        res))))
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


(provide 'core-search)
