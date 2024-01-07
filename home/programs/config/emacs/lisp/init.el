;;; init.el --- Core Emacs configuration
;;; Commentary:
;;; Code:

(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/company.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/copilot.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/dash.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/editor.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/eglot.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/git.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/irc.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/keys.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/notmuch.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/org.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/project.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/theme.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/vterm.el")

;;GeneralSettings
(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable the scroll bar
(tab-bar-mode -1) ; Disable the tab bar
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(global-display-line-numbers-mode t) ;; Show line numbers
(setq 
  make-backup-files nil ; Disable backup files
  auto-save-default nil ; Disable auto save
  display-line-numbers-type 'relative
  gc-cons-threshold 100000000 ;; 100mb
  read-process-output-max (* 1024 1024)) ;; 1mb

;; Direnv Configuration
(require 'direnv)
(direnv-mode 1)

;; Evil Configuration
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-collection-init '(calendar dired magit org org-roam)))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'ivy-switch-buffer
    "d" 'dired
    "e" 'erc-track-switch-buffer-other-window
    "f" 'counsel-find-file
    "F" 'counsel-rg
    "i" 'indent-region
    "k" 'kill-buffer
    "m" 'notmuch
    "q" 'kill-buffer-and-window
    "u" 'undo-tree-visualize

    ;;GitKeybindings
    "/" 'magit-status
    "gc" 'comment-or-uncomment-region
    "gl" 'magit-blame-addition
    "gb" 'blamer-mode

    ;; Projectile
    "pp" 'counsel-projectile-switch-project
    "pb" 'projectile-switch-to-buffer
    "pc" 'projectile-cleanup-known-projects
    "pf" 'counsel-projectile-find-file
    "pt" 'projectile-run-eshell

    ;; Eglot
    "lf" 'eglot-format-buffer
    "lr" 'eglot-rename
    "lgd" 'xref-find-definitions
    "lgr" 'xref-find-references
    "lga" 'eglot-code-actions
    "lgt" 'eglot-find-typeDefinition
    "lgh" 'eglot-inlay-hints-mode
    "lgo" 'eglot-code-action-organize-imports
    "lgq" 'eglot-code-action-quickfix

    "oa" 'org-agenda
    "os" 'org-schedule
    "od" 'org-deadline
    "oi" 'org-insert-structure-template
    "of" 'org-agenda-file-to-front
    "ot" 'org-insert-todo-heading
    "orl" 'org-roam-buffer-toggle
    "orf" 'org-roam-node-find
    "ori" 'org-roam-node-insert
    "ors" 'org-roam-db-sync
    "orr" 'org-roam-ui-mode
    "opl" 'org-present
    "opq" 'org-present-quit

    "tt" 'vterm
    "tm" 'multi-vterm
    "tn" 'multi-vterm-next
    "tp" 'multi-vterm-prev

    "ws" 'whitespace-mode
    "wc" 'whitespace-cleanup-mode)

  ;; Java-specific keybindings
  (evil-leader/set-key-for-mode 'java-moDe
    "jn" 'eglot-java-project-new
    "jb" 'eglot-java-project-build-refresh
    "jf" 'eglot-java-file-new
    "jm" 'eglot-java-run-main
    "jt" 'eglot-java-run-test
    "jul" 'eglot-java-upgrade-lsp-server
    "juj" 'eglot-java-upgrade-junit-jar)

  ;; Rust-specific keybindings
  (evil-leader/set-key-for-mode 'rustic-mode
    "b" 'rustic-cargo-build
    "c" 'rustic-cargo-check
    "r" 'rustic-cargo-run
    "t" 'rustic-cargo-test))

;;; init.el ends here
