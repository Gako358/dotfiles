;;; project.el --- Projectile configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-sort-order 'recently-active
        projectile-project-search-path '("~/Projects/" ("~/Projects/workspace/" . 1))))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(evil-leader/set-key
  "pp" 'counsel-projectile-switch-project
  "pb" 'projectile-switch-to-buffer
  "pc" 'projectile-cleanup-known-projects
  "pf" 'counsel-projectile-find-file
  "pt" 'projectile-run-eshell)

;;; project.el ends here