;;; copilot.el --- Copilot configuration for Emacs
;;; Commentary:
;;; Code:

;; Copilot Configuration
(let
    (
     (copilot-dir "~/.emacs.d/emacsCopilot")
     (copilot-file "~/.emacs.d/emacsCopilot/copilot.el"))
  ;; Check if the copilot.el file exists
  (when
      (file-exists-p copilot-file)
    ;; Add the directory to the load-path
    (add-to-list 'load-path copilot-dir)
    ;; Try to load the copilot module and catch any errors
    (condition-case err
        (progn
          (require 'copilot)
          (add-hook 'prog-mode-hook 'copilot-mode)
          (define-key copilot-completion-map
                      (kbd "C-p") 'copilot-accept-completion)
          (define-key copilot-mode-map
                      (kbd "<C-S-n>") #'copilot-next-completion
          (define-key copilot-mode-map
                      (kbd "<C-S-p>") #'copilot-previous-completion)))
      ;; If there's an error, print a message (you can also log or take other actions)
      (error
       (message "Failed to load copilot: %s" err)))))

;;; copilot.el ends here
