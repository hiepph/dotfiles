;;; init-keys --- Key bindings

;;; Commentary:
;;
;; Principle
;; - Be simple and mnemonic (The first principle is borrowed from @cmpitg.  Thanks!)
;; i.e. C-x t(ext editing) e(xpand region)
;;
;; - Be descriptive (Magit inspired)
;; i.e. Text editing: C-x t(ext editing) ...
;;   Text          | Pair
;;   ---------     | -----
;;   j: join lines | e: expand region
;;                 | d: delete surround

(require 'init-elpa)


;;; Code:


;; Simplify keybindings for some frequent tasks
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'helm-find-files)
(global-set-key (kbd "<f4>") #'save-buffers-kill-terminal)

(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-l") #'windmove-right)

(global-set-key (kbd "C-S-y") 'yank-and-indent)


;;; Hydra
(use-package hydra
  :ensure t
  :config
  :bind
  ("C-x t" . 'hydra-text/body)
  ("C-x c" . 'hydra-multiple-cursors/body)

  ("C-c f" . 'hydra-files/body)
  )


(defhydra hydra-text (:color red :idle 2)
  "
Text editing
------------
"
  ("j" crux-top-join-line "join lines" :column "Text")
  ("k" kill-whole-line "kill whole line")

  ("e" er/expand-region "expand-region" :column "Pair")
  ("d" sp-splice-sexp "delete surround")
  ("r" sp-rewrap-sexp "rewrap surround")
  ("f" sp-forward-sexp "forward to closed wrapping")
  ("b" sp-backward-sexp "backward to open wrapping")

  ("q" nil "quit" :column nil)
  )


(defhydra hydra-multiple-cursors (:color red :idle 2)
  "
Multiple cursors
----------------
"
  ;; ("c" mc/edit-lines "edit lines")
  ("f" mc/mark-next-like-this "next")
  ("b" mc/mark-previous-like-this "prev")
  ("d" mc/mark-all-like-this "all")
  )


(defhydra hydra-files (:color blue :idle 2)
  "
Files manipulation
------------------
"
  ("D" crux-delete-file-and-buffer "Delete file and buffer")
  ("R" crux-rename-file-and-buffer "Rename file and buffer")

  ("r" crux-recentf-find-file)

  ("q" nil "quit" :column nil)
  )


(provide 'init-keys)
;;; init-keys.el ends here
