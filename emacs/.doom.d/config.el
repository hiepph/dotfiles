;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Hiep Pham"
      user-mail-address "hiepph9@proton.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Notes/Roam")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;
;; Evil
;;
(use-package! evil
  :config
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t))

(use-package! evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode))

;;
;; Writings
;;
(use-package! org
  :init
  ;; Turn on image by default
  (setq org-startup-with-inline-images t)

  ;; Don't display full width of the image
  (setq org-image-actual-width nil)

  ;; Turn on latex view by default
  (setq org-startup-with-latex-preview t)

  ;; show special symbols
  (setq org-pretty-entities t)

  ;; Monday as the first day of the week
  (setq calendar-week-start-day 1)

  ;; Turn off tag inheritance
  (setq org-use-tag-inheritance nil)

  :hook
  (org-mode . (lambda () (setq-local tab-width 2)))
  (org-mode . (lambda () (auto-fill-mode))) ; auto new line at 'fill-column value

  :config
  (setq org-roam-directory (file-truename "~/Notes/Roam")))

(after! org
  (setq org-directory "~/Notes/Roam/")
  (setq org-agenda-files '("~/Notes/Roam"))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)" ; A task that needs doing and is ready to do
           "DOING(s)" ; A task that is in progress
           "BLOCKED(b)" ; Something is blocking this task
           "HOLD(h)" ; Maybe do it another time
           "REVIEW(r)" ; A task that needs review
           "|"
           "DONE(d)" ; A finished task
           "CANCELLED(c)" ; Task is killed, aborted or no longer applicable
           ))
        org-todo-keyword-faces
        '(("TODO" . "#009ddc")
          ("DOING" . "#5c9a55") ; green
          ("BLOCKED" . "#c44536") ; red
          ("HOLD" . "#e0a458") ; yellow
          ("REVIEW" . "#d9ae94")
          ("DONE" . "#b7b7a4") ; dim
          ("CANCELLED" . +org-todo-cancel))))

(use-package! markdown-mode
  :hook
  (markdown-mode . (lambda () (auto-fill-mode))))

(use-package! adoc-mode
  :hook
  (adoc-mode . (lambda () (auto-fill-mode))))

(after! flyspell
  (setenv "DICTIONARY" "en_GB-ise")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB-ise,en_GB")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB-ise,en_GB"))

;;
;; Ops
;;
(use-package! terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(put 'projectile-grep 'disabled nil)

;;
;; Languages
;;
(use-package! go-mode
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (setq gofmt-command "goimports"))

(setq-hook! 'python-mode-hook +format-with 'black)


;;
;; Projects
;;
(use-package! projectile
  :init
  (setq projectile-enable-caching nil))


;;
;; Navigation
;;
;; Don't try to guess dired buffer when I copy/move files.
(setq dired-dwim-target nil)

;;
;; Additional modules
;;
(load! "helpers.el")
(load! "keybindings.el")

;;
;; custom configurations for individual machine
;;
(load "~/customs/emacs.el" nil 'noerror)
