;;; theme.el --- Theme configuration
;;; Commentary:
;;; Code:

;; Themes
(require 'doom-one-theme)
(require 'doom-modeline)
(require 'all-the-icons)
(require 'all-the-icons-dired)
(require 'all-the-icons-ivy-rich)
(require 'nerd-icons)

(load-theme 'doom-one t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; Moodline
(doom-modeline-mode 1)
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
      doom-modeline-github-interval (* 30 60))

;; Font
(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (set-frame-font "Iosevka Nerd Font 11" nil t)
              (set-face-attribute 'mode-line nil :font "Iosevka Nerd Font 12" :height 100)
              (set-face-attribute 'company-tooltip nil :font "Iosevka Nerd Font 11" :height 100))))

;; Ivy Rich
(ivy-rich-mode 1)
(setq ivy-rich-path-style 'abbrev)

;; nerd-icons
(setq nerd-icons-font-family "Iosevka Nerd Font")

;;; theme.el ends here
