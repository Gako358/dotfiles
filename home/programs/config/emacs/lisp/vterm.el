;;; vterm.el --- Vterm Configuration
;;; Commentary:
;;; Code:

(require 'vterm)
(require 'multi-vterm)

(global-set-key
 (kbd "C-S-t") 'multi-vterm-project)

(global-set-key
 (kbd "C-S-b") 'multi-vterm-dedicated-toggle)
(setq multi-vterm-dedicated-window-height 37)

;;; vterm.el ends here
