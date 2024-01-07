;;; git.el --- Git Configuration
;;; Commentary:
;;; Code:

;; Git gutter configuration
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (fringe-mode '(8 . 8))
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

;; Git blame mode configuration
(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70))

;; Git commit mode configuration
(use-package forge
  :after magit)

(use-package evil-leader
  :config
  (evil-leader/set-key
    ;;GitKeybindings
    "/" 'magit-status
    "gc" 'comment-or-uncomment-region
    "gl" 'magit-blame-addition
    "gb" 'blamer-mode))

;;; git.el ends here