;;; direnv.el --- direnv configuration
;;; Commentary:
;;; Code:

;; Direnv Configuration
(require 'direnv)
(direnv-mode)
(add-hook 'eshell-directory-change-hook 'direnv-update-directory-environment)

;;; direnv.el ends here
