;;; editor.el --- Editor configuration
;;; Commentary:
;;; Code:

;; Flycheck (syntax checking)
(use-package flycheck
  :config
  (global-flycheck-mode)) ; Enable flycheck

;; Highlight current line
(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  :custom
  (highlight-thing-delay-seconds 0.5)
  (highlight-thing-case-sensitive-p t)
  (highlight-thing-ignore-list '("False" "True" "None")))

;; Ivy (completion)
(use-package ivy
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

;; Undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; EditorConfig
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  :hook
  (pdf-view-mode . (lambda ()
                     (when (bound-and-true-p display-line-numbers-mode)
                       (display-line-numbers-mode -1)))))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Whitespace
(use-package whitespace)
(use-package whitespace-cleanup-mode)

;;; editor.el ends here
