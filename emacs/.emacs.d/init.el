;; init.el --- Control all config files


;; Startup-hack
;; Avoid garbage collector at startup
;; Enable gchm-mode later in core-packages
(setq gc-cons-threshold most-positive-fixnum)

;; custom config location
(setq custom-file (concat user-emacs-directory "custom.el"))

;; save mini-buffer history
(savehist-mode 1)

;;
;; Use 'straight' for package managers
;; ref: https://github.com/raxod502/straight.el
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;
;; Core scripts
;;
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core)

(provide 'init)
