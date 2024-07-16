;;; $DOOMDIR/keybindings.el -*- lexical-binding: t; -*-

;; Editor
(map! :desc "Yank from killring"
      :leader
      "y" #'consult-yank-pop)

;; Org
(map! :desc "Org - Display inline image"
      :after org
      :map org-mode-map
      :localleader
      "v i" #'org-display-inline-images
      "v x" #'org-latex-preview)

;; TODO: use ~map!~
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "[ [" #'org-previous-visible-heading
 "] ]" #'org-next-visible-heading)

;; Project
(map! :desc "Projectile - Grep the project"
      :after projectile
      :map projectile-mode-map
      :localleader
      "/" #'projectile-grep)

;; Helpers
(map! :desc "Consult shell history"
      :leader
      "s h" #'~consult-shell-history)

;; Shell command
(map! "M->" #'~shell-command-insert)

;; Expand region
;; ref: https://github.com/magnars/expand-region.el
(map! :map evil-visual-state-map
      "+" #'er/expand-region)

(map! :desc "Expand Region"
      :map evil-normal-state-map
      :leader
      (:prefix-map ("+" . "expand-region")
       :desc "url" "u" #'er/mark-url
       :desc "comment" "c" #'er/mark-comment
       :desc "function" "f" #'er/mark-defun
       :desc "Python block" "b" #'er/mark-python-block
       :desc "Python block + decorator" "B" #'er/mark-python-block-and-decorator
       :desc "Org's element" "e" #'er/mark-org-element
       :desc "Org's code block" "c" #'er/mark-org-code-block
       :desc "Org's parent" "p" #'er/mark-org-parent))


;; Bookmarks
(map! :desc "Edit bookmarks"
      :map evil-normal-state-map
      :leader
      "b e" #'edit-bookmarks)

;; Toggle
(map! :desc "Visual Fill Column Mode"
      :leader
      "t w" #'visual-fill-column-mode
      "t W" #'global-visual-fill-column-mode)
