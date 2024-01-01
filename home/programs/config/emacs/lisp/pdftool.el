;;; pdftool.el --- pdf-tools configuration
;;; Commentary:
;;; Code:

;; Pdf Tools
(require 'pdf-tools)
(pdf-tools-install)
(setq-default pdf-view-display-size 'fit-page)
(setq pdf-annot-activate-created-annotations t)
(add-hook 'pdf-view-mode-hook
          (lambda
            ()
            (when
                (bound-and-true-p display-line-numbers-mode)
              (display-line-numbers-mode -1))))


;;; pdf-tools.el ends here
