;;; theme.el --- Theme configuration
;;; Commentary:
;;; Code:

(use-package doom-themes
      :ensure t
      :config
      (load-theme 'doom-one t)
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t))

(use-package doom-modeline
      :ensure t
      :init (doom-modeline-mode 1)
      :config
      (setq doom-modeline-height 35
            doom-modeline-bar-width 3
            doom-modeline-buffer-file-name-style 'truncate-with-project
            doom-modeline-buffer-encoding nil
            doom-modeline-buffer-modification-icon nil
            doom-modeline-buffer-state-icon nil
            doom-modeline-icon t
            doom-modeline-major-mode-icon t
            doom-modeline-minor-modes nil
            doom-modeline-persp-name nil
            doom-modeline-eglot t
            doom-modeline-github nil
            doom-modeline-github-interval (* 30 60)))

(use-package all-the-icons
      :ensure t)

(use-package all-the-icons-dired
      :ensure t
      :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy-rich
      :ensure t
      :init (all-the-icons-ivy-rich-mode 1))

(use-package nerd-icons
      :ensure t
      :config
      (setq nerd-icons-font-family "Iosevka Nerd Font"))

(add-hook 'after-make-frame-functions
      (lambda (f)
            (with-selected-frame f
                  (set-frame-font "Iosevka Nerd Font 11" nil t)
                  (set-face-attribute 'mode-line nil :font "Iosevka Nerd Font 12" :height 100)
                  (set-face-attribute 'company-tooltip nil :font "Iosevka Nerd Font 11" :height 100))))

;;; theme.el ends here