;;; vterm.el --- Vterm Configuration
;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :bind (("C-S-t" . multi-vterm-project)
         ("C-S-b" . multi-vterm-dedicated-toggle))
  :config
  (setq multi-vterm-dedicated-window-height 37))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-key
    "tt" 'vterm
    "tm" 'multi-vterm
    "tn" 'multi-vterm-next
    "tp" 'multi-vterm-prev))

;;; vterm.el ends here