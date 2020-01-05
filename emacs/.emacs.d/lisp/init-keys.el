;;; init-keys --- Key bindings

;;; Commentary:
;;
;; Principle
;; The first 2 principles are borrowed from @cmpitg.  Thanks!
;; - Be simple and mnemonic
;; - Descriptive

(require 'init-elpa)


;;; Code:


;;; Simplify keybindings for some frequent tasks
(global-set-key (kbd "<f2>") #'save-buffer)
(global-set-key (kbd "<f3>") #'helm-find-files)
(global-set-key (kbd "<f4>") #'save-buffer-kill-terrminal)


;;; Hydra
(use-package hydra
  :ensure t
  :config
  :bind
  ("C-x m" . 'hydra-window/body)
  ("C-x p" . 'hydra-pair/body)
  ("C-x t" . 'hydra-text/body)
  )


(defhydra hydra-window (:color blue :idle 2)
  "
Move: _h_, _j_, _k_, _l_ (vim style)
Split: _v_ert _x_:horz
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("v" split-window-right)
  ("x" split-window-below))


(defhydra hydra-pair (:color blue :idle 2)
  "
Pair processing
---------------
_e_xpand region
_d_elete surround
"
  ("e" er/expand-region)
  ("d" sp-splice-sexp))


(defhydra hydra-text (:color blue :idle 2)
  "
Text editing
------------
_j_oin lines"
  ("j" top-join-line))

(provide 'init-keys)
;;; init-keys.el ends here
