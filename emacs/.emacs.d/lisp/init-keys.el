;;; init-keys --- Key bindings

;;; Commentary:
;;
;; Principle
;; - Be simple and mnemonic (The first principle is borrowed from @cmpitg.  Thanks!)
;; - Be descriptive as Magit (Show a window with hints)

(require 'init-elpa)


;;; Code:


;; Simplify keybindings for some frequent tasks
;; Buffer
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'helm-find-files)
(global-set-key (kbd "<f4>") #'save-buffers-kill-terminal)

(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-l") #'windmove-right)

;; Text Editing
(global-set-key (kbd "C-S-y") 'yank-and-indent)
(global-set-key (kbd "C-^") 'crux-top-join-line)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
(global-set-key (kbd "C-=") 'er/expand-region)


;;; Hydra
(use-package hydra
  :ensure t
  :config
  :bind
  ("C-x e" . 'hydra-pair/body)
  ("C-x c" . 'hydra-multiple-cursors/body)

  ("C-c f" . 'hydra-files/body)
  )


(defhydra hydra-pair (:color red :idle 1)
  "
Text editing
------------
"
  ("u" crux-upcase-region "upcase" :column "Text")
  ("c" crux-capitalize-region "capitalize")
  ("d" crux-downcase-region "downcase")
  ("j" crux-top-join-line "join")
  ("e" er/expand-region "expand region" :column "Pair")
  ("d" sp-splice-sexp "delete surround")
  ("r" sp-rewrap-sexp "rewrap")
  ("f" sp-forward-sexp "forward to matching paren")
  ("b" sp-backward-sexp "backward to matching paren")
  ("q" nil "quit" :column nil)
  )


(defhydra hydra-multiple-cursors (:color red :idle 1)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))


(defhydra hydra-files (:color blue :idle 1)
  "
Files manipulation
------------------
"
  ("D" crux-delete-file-and-buffer "Delete file and buffer")
  ("R" crux-rename-file-and-buffer "Rename file and buffer")
  ("q" nil "quit" :column nil)
  )


(provide 'init-keys)
;;; init-keys.el ends here
