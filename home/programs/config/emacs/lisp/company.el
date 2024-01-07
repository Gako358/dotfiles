;;; company.el --- Company Mode Configuration
;;; Commentary:
;;; Code:

;; Enable Completion
(defvar company-backends
  "Company backends variable")

(defvar company-idle-delay
  "Company idle delay variable")

(use-package company
  :demand t
  :config
  (setq
   company-backends '(company-capf company-files company-dabbrev)
   company-idle-delay 0.1)
  :init
  :hook (after-init . global-company-mode))

(defvar company-box-icons-alist
  "Company box icons alist variable")
(defvar company-box-backends-colors
  "Company box backends colors variable")
(defvar company-box-show-single-candidate
  "Company box show single candidate variable")
(defvar company-box-max-candidates
  "Company box max candidates variable")
(defvar company-box-doc-delay
  "Company box doc delay variable")
(defvar company-box-enable-icon
  "Company box enable icon variable")
(defvar company-box-scrollbar
  "Company box scrollbar variable")

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

(defvar company-quickhelp-delay
  "Company quickhelp delay variable")

(use-package company-quickhelp
  :demand t
  :config
  (setq company-quickhelp-delay 0.1)
  :after company
  :init
  :hook (company-mode . company-quickhelp-mode))

;;; company.el ends here
