;;; dash.el --- Dashboard Mode Configuration
;;; Commentary:
;;; Code:

(require 'dashboard)

(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title "Welcome back Master")
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(dashboard-setup-startup-hook)

;;; dash.el ends here
