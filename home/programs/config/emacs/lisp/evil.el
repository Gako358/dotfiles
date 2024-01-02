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
  "F"'counsel-git-grep
  "i"'indent-region
  "m"'notmuch
  "k"'kill-buffer
  "q"'kill-all-buffers-except-current
  "w"'save-buffer
  "x"'counsel-M-x
  "d"'dired
  "t"'vterm
  "p"'projectile-command-map
  "/"'magit-status
  "gc"'comment-or-uncomment-region

  ;;OrgKeybindings
  "oa"'org-agenda
  "os"'org-schedule
  "od"'org-deadline
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
  "lws"'lsp-ui-sideline-mode
  "lwd"'lsp-ui-doc-mode
  "lwr"'lsp-ui-peek-find-references
  "lwf"'lsp-ui-peek-find-definitions
  "lwa"'lsp-ui-peek-find-implementation
  "lwh"'lsp-ui-doc-glance
  "lwe"'lsp-treemacs-errors-list
  "lf"'lsp-format-buffer
  "lpf"'blacken-buffer
  "lxf"'xml-pretty-print
  "lr"'lsp-rename
  "lR"'lsp-workspace-restart
  "la"'lsp-execute-code-action
  "ld"'lsp-describe-thing-at-point
  "lgt"'lsp-goto-type-definition
  "lgi"'lsp-goto-implementation
  "lsh"'lsp-symbol-highlight
  "lwa"'lsp-workspace-folders-add
  "lwr"'lsp-workspace-folders-remove
  "lws"'lsp-workspace-folders-switch

  ;;JavaKeybindings
  "jo"'lsp-java-organize-imports
  "jbp"'lsp-java-build-project
  "jupc"'lsp-java-update-project-configuration
  "jan"'lsp-java-actionable-notifications
  "juus"'lsp-java-update-user-settings
  "jus"'lsp-java-update-server
  "jgt"'lsp-java-generate-to-string
  "jgeh"'lsp-java-generate-equals-and-hash-code
  "jgo"'lsp-java-generate-overrides
  "jggs"'lsp-java-generate-getters-and-setters
  "jth"'lsp-java-type-hierarchy
  "jec"'lsp-java-extract-to-constant
  "jaum"'lsp-java-add-unimplemented-methods
  "jcp"'lsp-java-create-parameter
  "jcf"'lsp-java-create-field
  "jcl"'lsp-java-create-local
  "jem"'lsp-java-extract-method
  "jai"'lsp-java-add-import
  "jtb"'lsp-jt-browser
  "jro"'lsp-jt-report-open
  "jlm"'lsp-jt-lens-mode

  ;;DAPJavaKeybindings
  "jdd"'dap-java-debug
  "jrtm"'dap-java-run-test-method
  "jdtm"'dap-java-debug-test-method
  "jrtc"'dap-java-run-test-class
  "jdtc"'dap-java-debug-test-class)

(evil-leader/set-key-for-mode 'notmuch-mode
  "u"'+notmuch/update
  "d"'+notmuch/search-delete)

;;; evil.el ends here
