;;
;; ref: https://github.com/emacs-evil/evil
;;
(use-package evil
  :after magit
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-substitute-highlight-all nil)
  (setq evil-ex-search-persistent-highlight nil)
  :config
  (evil-mode 1))


;;
;; Quick key sequence to escape
;;
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "<escape>")
  :config
  (evil-escape-mode))

;;
;; Pre-keybindings for some common Emacs modes
;; ref: https://github.com/emacs-evil/evil-collection
;;
(setq evil-want-keybinding nil)
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-magit-state 'normal)
  :custom
  ;; enable Evil in minibuffer
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'compile))


;;
;; Evil keybindings for Magit
;;
;; Notes: This is included inside evil-collection, but it doesn't work
;; so I have to add this anyway.
;;
(use-package evil-magit
  :after (evil magit))

;;
;; Surround
;; ref: https://github.com/emacs-evil/evil-surround
;;
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


;;
;; Sneak (emulate vim-sneak)
;; ref: https://github.com/hlissner/evil-snipe
;;
(use-package evil-snipe
  :init
  (setq evil-snipe-scope 'buffer)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;;
;; Easymotion
;;
(use-package evil-easymotion
  :config
  (evilem-default-keybindings "\\"))

;;
;; Search for selected region (especially word)
;;
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;;
;; Evil fringe mark - show markers (e.g. `m m')
;; ref: https://github.com/Andrew-William-Smith/evil-fringe-mark
;;
(use-package evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode 1))


;;
;; Commenter
;; ref: https://github.com/linktohack/evil-commentary
;;
(use-package evil-commentary
  :config
  (evil-commentary-mode))


;; Multiple cursors
;; ref: https://github.com/gabesoft/evil-mc
;; keybinding: https://github.com/gabesoft/evil-mc/blob/master/evil-mc.el
;;
;; * Note:
;; evil-mc and visual selection mode can play nicely together
;; Suppose I selected some text in visual mode, then g-r-I to active multiple cursors
(use-package evil-mc
  :config
  (global-evil-mc-mode))


;;
;; Set variables depends on mode
;;

(add-hook 'yaml-mode-hook (lambda ()
                            (setq evil-shift-width 2)))


(provide 'core-evil)
