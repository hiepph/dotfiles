(require 'init-elpa)

(require 'saveplace)
(require-package 'autopair)
(require-package 'rainbow-delimiters)

; Autopair
;; enable autopair in all buffers
(autopair-global-mode)
;; except [org]
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (autopair-mode -1)))

; Trailing white space
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

(provide 'init-editing)
