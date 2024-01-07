;;; company.el --- Company Mode Configuration
;;; Commentary:
;;; Code:

;; Enable Completion
(use-package company
  :demand t
  :config
  (setq
   company-backends '(company-capf company-files company-dabbrev)
   company-idle-delay 0.1)
  :init
  :hook (after-init . global-company-mode))

(use-package company-box
  :demand t
  :config
  (setq
   company-box-icons-alist 'company-box-icons-all-the-icons
   company-box-backends-colors nil
   company-box-show-single-candidate t
   company-box-max-candidates 50
   company-box-doc-delay 0.1
   company-box-enable-icon t
   company-box-scrollbar t)
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :demand t
  :config
  (setq company-quickhelp-delay 0.1)
  :after company
  :init
  :hook (company-mode . company-quickhelp-mode))

;;; company.el ends here
