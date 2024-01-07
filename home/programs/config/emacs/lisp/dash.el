;;; dash.el --- Dashboard Mode Configuration
;;; Commentary:
;;; Code:

;; Defvar all the dashboard variables
(defvar dashboard-startup-banner
  "Startup banner for dashboard.")
(defvar dashboard-banner-logo-title
  "Banner logo title for dashboard.")
(defvar dashboard-set-file-icons
  "Set file icons for dashboard.")
(defvar dashboard-center-content
  "Center content for dashboard.")
(defvar dashboard-items
  "Items for dashboard.")

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
