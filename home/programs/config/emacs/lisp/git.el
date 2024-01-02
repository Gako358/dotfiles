;;; git.el --- Git Configuration
;;; Commentary:
;;; Code:

(require 'git-gutter)
(require 'git-gutter-fringe)

(add-hook 'prog-mode-hook 'git-gutter-mode)
(setq git-gutter:update-interval 0.05)

(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

;;; git.el ends here
