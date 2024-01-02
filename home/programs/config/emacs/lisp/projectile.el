;;; projectile.el --- Projectile configuration
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


;;; projectile.el ends here
