(require 'init-elpa)
(require 'init-func)

;;
;; ACME
;; ref: https://github.com/cmpitg/wand
;;
(use-package wand
  :ensure t
  :config
  (setq wand:*rules*
        (list
         (wand:create-rule :match (rx bol (0+ " ") "!")
                           :capture :after
                           :action #'~acme!)
         (wand:create-rule :match (rx bol (0+ " ") "$")
                           :capture :after
                           :action #'~acme$)
         (wand:create-rule :match (rx bol (0+ " ") "<")
                           :capture :after
                           :action #'~acme<)
         (wand:create-rule :match (rx bol (0+ " ") "http")
                           :capture :whole
                           :action #'browse-url-firefox)
         )))


(provide 'init-literate)
