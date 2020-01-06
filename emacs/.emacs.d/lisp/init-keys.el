;;; init-keys --- Key bindings

;;; Commentary:
;;
;; Principle
;; - Be simple and mnemonic (The first principle is borrowed from @cmpitg.  Thanks!)
;; i.e. C-x t(ext editing) e(xpand region)
;;
;; - Be descriptive (Magit inspired)
;; i.e. Windmove
;;   window    | split
;;   --------- | -----
;;   h: ←      | v: vertical
;;   j: ↓      | x: horizontal
;;   k: ↑      |
;;   l: →      |

(require 'init-elpa)


;;; Code:


;;; Simplify keybindings for some frequent tasks
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'helm-find-files)
(global-set-key (kbd "<f4>") #'save-buffers-kill-terminal)


;;; Hydra
(use-package hydra
  :ensure t
  :config
  :bind
  ("C-x m" . 'hydra-window/body)
  ("C-x t" . 'hydra-text/body)
  ("C-x c" . 'hydra-multiple-cursors/body)
  )


(defhydra hydra-window (:color red :idle 2)
  "
Windmove
--------
"
  ("h" windmove-left  "←" :column "Window")
  ("j" windmove-down "↓")
  ("k" windmove-up "↑")
  ("l" windmove-right "→")

  ("v" split-window-right "vertical" :column "Split")
  ("x" split-window-below "horizontal")

  ("q" nil "quit" :column nil))


(defhydra hydra-text (:color red :idle 2)
  "
Text editing
------------
"
  ("j" top-join-line "join lines" :column "Text")

  ("e" er/expand-region "expand-region" :column "Pair")
  ("d" sp-splice-sexp "delete surround")

  ("q" nil "quit" :column nil)
  )


(defhydra hydra-multiple-cursors (:color red :idle 2)
  "
Multiple cursors
----------------
"
  ("c" mc/edit-lines "edit lines")
  ("f" mc/mark-next-like-this "next")
  ("b" mc/mark-previous-like-this "prev")
  ("d" mc/mark-all-like-this "all")
  )

(global-set-key (kbd "C-S-y") 'yank-and-indent)



(provide 'init-keys)
;;; init-keys.el ends here
