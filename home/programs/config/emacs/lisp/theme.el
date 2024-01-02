;;; theme.el --- Theme configuration
;;; Commentary:
;;; Code:

;; Doom Themes
(require 'doom-themes)
(load-theme 'doom-one t)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-height 35)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-continuous-word-count-modes '
      (markdown-mode gfm-mode org-mode text-mode))
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-indent-info t)
(setq doom-modeline-total-line-number t)
(setq doom-modeline-github t)
(setq doom-modeline-github-interval
      (* 10 60))

;; Tabspaces
(require 'tabspaces)
(setq tabspaces-use-filtered-buffers-as-default t)
(setq tabspaces-default-tab "Default")
(setq tabspaces-remove-to-default t)
(setq tabspaces-include-buffers '("*scratch*"))
(setq tabspaces-initialize-project-with-todo t)
(setq tabspaces-todo-file-name "project-todo.org")

;;Tabspaces sessions
(setq tabspaces-session t)
(setq tabspaces-session-auto-restore t)

;; Font
(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (set-frame-font "Iosevka Nerd Font 11" nil t)
              (set-face-attribute 'company-tooltip nil :font "Iosevka Nerd Font 11" :height 100))))

;;; theme.el ends here
