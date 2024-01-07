;;; init.el --- Core Emacs configuration
;;; Commentary:
;;; Code:

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

;; Evil
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
    "u" 'undo-tree-visualize))

(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/company.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/copilot.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/dash.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/direnv.el")
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

;;; init.el ends here
