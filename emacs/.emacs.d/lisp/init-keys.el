;;
;; Principle
;; - Be simple and mnemonic (The first principle is borrowed from @cmpitg.  Thanks!)
;; - Be descriptive as Magit (Show a window with hints)
;;

(require 'init-elpa)

;;
;; Simplify keybindings for some frequent tasks
;;
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'find-file)
(global-set-key (kbd "<f4>") #'save-buffers-kill-terminal)
(global-set-key (kbd "<f5>") #'eval-buffer)



;;
;; Evil keybindings
;; ref: https://github.com/emacs-evil/evil
;;
(use-package evil
  :ensure t
  :init
  (evil-mode 1))



;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-l") #'windmove-right)

;; ;; Text Editing
;; (global-set-key (kbd "C-S-y") 'yank-and-indent)
;; (global-set-key (kbd "C-S-j") 'crux-top-join-line)
;; (global-set-key (kbd "C-k") 'crux-smart-kill-line)
;; (global-set-key (kbd "C-=") 'er/expand-region)




;; ;;; Hydra
;; (use-package hydra
;;   :ensure t
;;   :config
;;   :bind
;;   ("C-x t" . 'hydra-text/body)
;;   ("C-x e" . 'hydra-expand/body)
;;   ("C-x p" . 'hydra-pair/body)
;;   ("C-x c" . 'hydra-multiple-cursors/body)

;;   ("C-c f" . 'hydra-files/body)
;;   )


;; (defhydra hydra-expand (:color red)
;;   "
;; Expand region
;; -------------
;; "
;;   ("w" er/mark-word "mark-word")
;;   ("s" er/mark-symbol "mark-symbol")
;;   ;; ("" er/mark-symbol-with-prefix "mark-symbol-with-prefix")
;;   ;; ("" er/mark-next-accessor "mark-next-accessor")
;;   ;; ("" er/mark-method-call "mark-method-call")
;;   ("i" er/mark-inside-quotes "mark-inside-quotes")
;;   ("o" er/mark-outside-quotes "mark-outside-quotes")
;;   ("I" er/mark-inside-pairs "mark-inside-pairs")
;;   ("O" er/mark-outside-pairs "mark-outside-pairs")
;;   ;; ("c" er/mark-comment "mark-comment")
;;   ("u" er/mark-url "mark-url")
;;   ("e" er/mark-email "mark-email")
;;   ("f" er/mark-defun "mark-defun")

;;   ("q" nil "quit" :column nil)
;;   )


;; (defhydra hydra-pair (:color red)
;;   "
;; Pair
;; ----
;; "
;;   ("d" sp-splice-sexp "delete surround")
;;   ("r" sp-rewrap-sexp "rewrap")
;;   )


;; (defhydra hydra-text (:color red)
;;   "
;; Text editing
;; ------------
;; "
;;   ("u" crux-upcase-region "upcase")
;;   ("c" crux-capitalize-region "capitalize")
;;   ("d" crux-downcase-region "downcase")
;;   )


;; (defhydra hydra-multiple-cursors (:color red)
;;   "
;;  Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
;; ------------------------------------------------------------------
;;  [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
;;  [Click] Cursor at point       [_q_] Quit"
;;   ("l" mc/edit-lines :exit t)
;;   ("a" mc/mark-all-like-this :exit t)
;;   ("n" mc/mark-next-like-this)
;;   ("N" mc/skip-to-next-like-this)
;;   ("M-n" mc/unmark-next-like-this)
;;   ("p" mc/mark-previous-like-this)
;;   ("P" mc/skip-to-previous-like-this)
;;   ("M-p" mc/unmark-previous-like-this)
;;   ("s" mc/mark-all-in-region-regexp :exit t)
;;   ("0" mc/insert-numbers :exit t)
;;   ("A" mc/insert-letters :exit t)
;;   ("<mouse-1>" mc/add-cursor-on-click)
;;   ;; Help with click recognition in this hydra
;;   ("<down-mouse-1>" ignore)
;;   ("<drag-mouse-1>" ignore)
;;   ("q" nil))


;; (defhydra hydra-files (:color blue)
;;   "
;; Files manipulation
;; ------------------
;; "
;;   ("d" crux-delete-file-and-buffer "Delete file and buffer")
;;   ("r" crux-rename-file-and-buffer "Rename file and buffer")
;;   ("q" nil "quit" :column nil)
;;   )


(provide 'init-keys)
