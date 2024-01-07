;;; direnv.el --- direnv configuration
;;; Commentary:
;;; Code:

;; Direnv Configuration
(use-package direnv
  :ensure t
  :init
  :hook ('eshell-directory-change-hook . direnv-update-directory-environment))

;;; direnv.el ends here
