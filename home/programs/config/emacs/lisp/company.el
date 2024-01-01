;;; company.el --- Company Mode Configuration
;;; Commentary:
;;; Code:

;; Enable Completion
(require 'company)
(require 'company-box)
(defun setup-company-mode
    ()
  "Setup company mode."
  (yas-minor-mode-on)
  (company-box-mode 1))

(add-hook 'company-mode-hook 'setup-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-tooltip-align-annotations t)

;;; company.el ends here
