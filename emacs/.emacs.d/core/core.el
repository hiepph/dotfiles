;;
;; Basic
;;

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; replace highlight text with typing action
(delete-selection-mode 1)

;; Set default directory when open
(setq default-directory "~/")

;; Set *scratch* mode default to fundamental
(setq initial-major-mode 'fundamental-mode)


;;
;; Packages
;;

;; use use-package as package bootstraping
(straight-use-package 'use-package)
;; auto specifying `:straight t` to automatically install missing packages
(setq straight-use-package-by-default t)

;;
;; Garbage collector hacks
;;
(use-package gcmh
  :straight (gcmh :type git :host github :repo "emacsmirror/gcmh")
  :init
  (gcmh-mode 1))

;;
;; Modules
;;

;; Editor
(require 'core-editor)

;; Evil keybindings
(require 'core-evil)

;; Languages
(require 'core-languages)

;; project management
;; buffers
;; dired
(require 'core-project)

;;
(require 'core-search)

;; Themes, fonts, etc.
(require 'core-ui)

;; Keybindings to rule them all
(require 'core-keybindings)

;; custom config for individual machine
(load "~/customs/emacs.el" nil 'noerror)

(provide 'core)
