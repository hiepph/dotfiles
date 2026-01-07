;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Window title
(setq-default frame-title-format '("%f"))

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
(setq display-line-numbers-type 'relative)

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
;; Editor
;;

(use-package! evil
  :config
  (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t))

(use-package! evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode))

(use-package! lispyville
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode))

;; Auto-save when a buffer loses focus
(use-package! super-save
  :config
  (super-save-mode +1))

;;
;; Org
;;

(use-package! org
  :init
  ;; Turn on image by default
  ;; (setq org-startup-with-inline-images t)

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
  (org-mode . (lambda () (visual-fill-column-mode)))
  (org-mode . (lambda () (setq-local fill-column 120)))
  (org-mode . (lambda () (org-bullets-mode)))

  :config
  (setq org-roam-directory (file-truename "~/Notes/Roam")))

(after! org
  (setq org-directory "~/Notes/Roam/")
  (setq org-agenda-files '("~/Notes/Roam"))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                ; A task that needs doing and is ready to do
           "DOING(s)"               ; A task that is in progress
           "BLOCKED(b)"             ; Something is blocking this task
           "HOLD(h)"                ; Maybe do it another time
           "CANCELLED(c)"      ; Task is killed, aborted or no longer applicable
           "ARCHIVED(a)"       ; Deprecated, but still contains valuable information
           "REVIEW(r)"       ; required reviewing and refactoring
           "?(?)"              ; Should I do this?
           "DONE(d)"           ; A finished task
           ))
        org-todo-keyword-faces
        '(("TODO" . "#009ddc")          ; blue
          ("DOING" . "#5c9a55")         ; green
          ("BLOCKED" . "#c44536")       ; red
          ("HOLD" . "#e0a458")          ; yellow
          ("CANCELLED" . "#540b0e")     ; dark brown
          ("ARCHIVED" . "#e07a5f")      ; orange
          ("REVIEW" . "#588157")        ; dark green
          ("?" . "#000000")             ; black
          ("DONE" . "#b7b7a4")          ; grey
          ))

  ;; turn off eye-candy to improve speed
  ;; (remove-hook 'org-mode-hook #'org-superstar-mode)
  ;; (setq org-fontify-quote-and-verse-blocks nil
  ;;       org-fontify-whole-heading-line nil
  ;;       org-hide-leading-stars nil)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((eshell . t)
                                 (emacs-lisp . t)
                                 (mermaid . t)
                                 (python . t)
                                 (scheme . t)
                                 (ruby . t))))

;; Disable marginalia-mode to prevent vertico slow down org-mode
;; ref: https://github.com/doomemacs/doomemacs/issues/6622
(use-package! marginalia-mode
  :disabled)

;; Increase GC's threshold to improve GC performance
;; ref: https://github.com/doomemacs/doomemacs/issues/3108
;; (after! gcmh
;;   (setq gcmh-high-cons-threshold (* 32 (expt 2 20))))

(use-package! ob-mermaid
  :config
  (setq ob-mermaid-cli-path "~/opt/js/node_modules/.bin/mmdc"))

(use-package! markdown-mode
  :hook
  (markdown-mode . (lambda () (setq-local fill-column 120)))
  (markdown-mode . (lambda () (visual-fill-column-mode))))

(use-package! adoc-mode
  :hook
  (adoc-mode . (lambda () (auto-fill-mode))))

(after! flyspell
  (setenv "DICTIONARY" "en_GB")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB")

  ;; Ignore all uppercase acronyms
  (setq flyspell-abbrev-p t))

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

;; Associate file types with file modes.
(add-to-list 'auto-mode-alist '("\\Tiltfile\\'" . python-mode))

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
;; (setq dired-dwim-target nil)

;;
;; Shells
;;

(setq eshell-buffer-maximum-lines 8192)

;; Command works in shell, but not in Emacs?
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;
;; Additional modules
;;
(load! "helpers.el")
(load! "keybindings.el")

;;
;; custom configurations for individual machine
;;
(load "~/customs/emacs.el" nil 'noerror)
