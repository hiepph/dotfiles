(require 'init-elpa)

(require 'saveplace)
(require-package 'company)
(require-package 'company-quickhelp)
(require-package 'undo-tree)
(require-package 'autopair)
(require-package 'expand-region)
(require-package 'rainbow-delimiters)
(require-package 'pos-tip)
(require-package 'popup)
(require-package 'whitespace)
(require-package 'aggressive-indent)


;; replace highlight text with typing action
(delete-selection-mode 1)


;; Disable tabs mode
(setq-default indent-tabs-mode nil)

;; View tabs as 4 spaces
(setq default-tab-width 4)

;; Tab as 4 spaces
(setq tab-width 4)


;; Autopair
;; enable autopair in all buffers
(autopair-global-mode)

;; auto-wrap word into pair
(setq autopair-autowrap t)

;; except [org]
(add-hook 'org-mode-hook
          #'(lambda ()
              (autopair-mode -1)))

;; Triple quote in python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; prevent '<' being paired in Rust
;; (add-hook 'rust-mode-hook
;;           #'(lambda ()
;;               (push ?<
;;                     (getf autopair-dont-pair :code))))


;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)


;; Truncate lines instead of wrap-lines
;; (setq-default truncate-lines t)


;; Auto-delete
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq whitespace-line-column 100)
;; `lines-tail` highlight part of lines that goes beyond ‘whitespace-line-column’ (default: 80)
;; `trailing` highlight trailing white-spaces
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)


;; Auto-indentation
(electric-indent-mode 1)

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; Auto indent for pasted code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-S-y") 'yank-and-indent)

;; Aggressive (force) indent block of code
;; (global-aggressive-indent-mode)


;; Highlights matching parenthesis
(show-paren-mode 1)
;; Rainbow parentheses
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings.
;; By default, C-s runs isearch-forward, so this swaps the bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; Fast comment toggle
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Join line and next line
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-^") 'top-join-line)


;; Auto complete
(add-hook 'after-init-hook 'global-company-mode)
;; show help
(company-quickhelp-mode 1)
;; bind 'company-complete to C-\
(global-set-key (kbd "C-\\") 'company-complete)
;; Activate after 2 chars
(setq company-minimum-prefix-length 2)
;; Zero-delay
(setq company-idle-delay 0)


;; Flycheck
(require-package 'flycheck)
;; Install back-end checker
;; pip install pylint

(global-flycheck-mode
 ;; bind on-off switch
 (global-set-key [f2] 'flycheck-mode)


;; Undo tree
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
;; Undo
(global-set-key (kbd "C-/") 'undo)
;; Redo
(global-set-key (kbd "C-S-/") 'redo)


;; Go to matching parenthesis
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)


;; Tab


(provide 'init-editing)
