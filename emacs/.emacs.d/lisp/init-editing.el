(require 'init-elpa)

(require 'saveplace)
(require-package 'undo-tree)
(require-package 'autopair)
(require-package 'expand-region)
(require-package 'rainbow-delimiters)
(require-package 'auto-complete)

;; Disable tabs mode
;; (setq-default indent-tabs-mode nil)

;; View tabs as 4 spaces
(setq default-tab-width 4)

; Autopair
;; enable autopair in all buffers
(autopair-global-mode)
;; except [org]
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (autopair-mode -1)))

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

; Trailing white space
;; Load white space mode
; (autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
; (autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
;; show
(setq-default show-trailing-whitespace t)
;; Auto-delete
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlights matching parenthesis
(show-paren-mode 1)
;; Rainbow parentheses
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
; (global-set-key (kbd "C-M-s") 'isearch-forward)
; (global-set-key (kbd "C-M-r") 'isearch-backward)

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

;; Auto complete config
(ac-config-default)
;; also enable inside string
(setq ac-disable-faces nil)

;; Undo tree
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
;; Undo
(global-set-key (kbd "C-/") 'undo)
;; Redo
(global-set-key (kbd "C-S-/") 'redo)

(provide 'init-editing)
