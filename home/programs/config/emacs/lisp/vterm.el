;;; vterm.el --- Vterm Configuration
;;; Commentary:
;;; Code:

;; Vterm Configuration
(require 'vterm)
(defun project-vterm ()
  "Open a vterm in the project's root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (vterm)))

;;; vterm.el ends here
