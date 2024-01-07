;;; dash.el --- Dashboard Mode Configuration
;;; Commentary:
;;; Code:

;; Dashboard Configuration
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Welcome back Master"
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :init
  :hook (after-init . dashboard-setup-startup-hook))

;;; dash.el ends here
