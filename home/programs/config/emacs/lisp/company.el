;;; company.el --- Company Mode Configuration
;;; Commentary:
;;; Code:

;; Enable Completion
(require 'company)
(require 'company-box)
(require 'company-quickhelp)
(setq company-backends '(company-capf company-files company-dabbrev))
(setq company-idle-delay 0.1)

;; Enable Company Quickhelp
(setq company-quickhelp-delay 0.1)

;; Enable Company Box
(setq company-box-icons-alist 'company-box-icons-all-the-icons)
(setq company-box-backends-colors nil)
(setq company-box-show-single-candidate t)
(setq company-box-max-candidates 50)
(setq company-box-doc-delay 0.1)
(setq company-box-enable-icon t)
(setq company-box-scrollbar t)

(add-hook 'company-mode-hook 'company-box-mode)
(add-hook 'company-mode-hook 'company-quickhelp-mode)
(add-hook 'after-init-hook 'global-company-mode)

;;; company.el ends here
