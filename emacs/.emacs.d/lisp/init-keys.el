;;
;; Principle
;; - Be simple and mnemonic
;; - Be descriptive (e.g. show a window with hints)
;;

(require 'init-elpa)

;;
;; Evil keybinding
;; ref: https://github.com/emacs-evil/evil
;;
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
)

;;
;; Evil escape
;; quickly escape to normal state using 'fd'
;; ref: https://github.com/syl20bnr/evil-escape
;;
(use-package evil-escape
  :ensure t
  :init
  (evil-escape-mode))


(provide 'init-keys)
