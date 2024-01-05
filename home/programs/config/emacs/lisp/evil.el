;;; evil.el --- Evil configuration
;;; Commentary:
;;; Code:

(require 'evil)
(evil-mode 1)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(evil-collection-init '(calendar dired magit org org-roam))

;;Settheleaderkeytospace
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b"'ivy-switch-buffer
  "f"'counsel-find-file
  "F"'counsel-rg
  "i"'indent-region
  "k"'kill-buffer
  "m"'notmuch
  "q"'kill-buffer-and-window
  "w"'save-buffer
  "x"'counsel-M-x
  "d"'dired

  ;;ProjectKeybindings
  "pp"'counsel-projectile-switch-project
  "pb"'projectile-switch-to-buffer
  "pc"'projectile-cleanup-known-projects
  "pf"'counsel-projectile-find-file
  "pt"'project-eshell

  ;;VtermKeybindings
  "tt"'vterm
  "tm"'multi-vterm
  "tn"'multi-vterm-next
  "tp"'multi-vterm-prev

  ;;GitKeybindings
  "/"'magit-status
  "gc"'comment-or-uncomment-region
  "gl"'magit-blame-addition
  "gb"'blamer-mode

  ;;OrgKeybindings
  "oa"'org-agenda
  "os"'org-schedule
  "od"'org-deadline
  "oi"'org-insert-structure-template
  "of"'org-agenda-file-to-front
  "ot"'org-insert-todo-heading
  "orl"'org-roam-buffer-toggle
  "orf"'org-roam-node-find
  "ori"'org-roam-node-insert
  "ors"'org-roam-db-sync
  "orr"'org-roam-ui-mode
  "opl"'org-present
  "opq"'org-present-quit

  ;;LSPKeybindings
  "lf" 'eglot-format-buffer
  "lr" 'eglot-rename
  "lgd" 'xref-find-definitions
  "lgr" 'xref-find-references
  "lga" 'eglot-code-actions
  "lgt" 'eglot-find-typeDefinition
  "lgh" 'eglot-inlay-hints-mode
  "lgo" 'eglot-code-action-organize-imports
  "lgq" 'eglot-code-action-quickfix

  ;;JavaKeybindings
  "jn" 'eglot-java-project-new
  "jb" 'eglot-java-project-build-refresh
  "jf" 'eglot-java-file-new
  "jrm" 'eglot-java-run-main
  "jrt" 'eglot-java-run-test
  "jul" 'eglot-java-upgrade-lsp-server
  "juj" 'eglot-java-upgrade-junit-jar)


(evil-leader/set-key-for-mode 'notmuch-mode
  "u"'+notmuch/update
  "d"'+notmuch/search-delete)

;;; evil.el ends here
