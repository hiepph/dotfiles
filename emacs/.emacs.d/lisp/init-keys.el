;;; init-keys --- Key bindings

;;; Commentary:
;;
;; Principle
;; The first 2 principles are borrowed from @cmpitg.  Thanks!
;; - Be simple and mnemonic
;; - Descriptive

(require 'init-elpa)


;;; Code:
(use-package hydra
  :ensure t
  :config
  :bind
  ("C-w-m" . 'hydra-window/body)
  )


(defhydra hydra-window (:color red :idle 2)
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


(provide 'init-keys)
;;; init-keys.el ends here
