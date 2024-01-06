;;; project.el --- Projectile configuration
;;; Commentary:
;;; Code:

;; Projectile
(require 'projectile)
(require 'counsel-projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)
(setq projectlile-sort-order 'recently-active)
(setq projectile-project-search-path '
      ("~/Projects/"
       ("~/Projects/workspace/" . 1)))

(evil-leader/set-key
  "pp"'counsel-projectile-switch-project
  "pb"'projectile-switch-to-buffer
  "pc"'projectile-cleanup-known-projects
  "pf"'counsel-projectile-find-file
  "pt"'project-eshell)


;;; project.el ends here
