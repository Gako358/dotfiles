;;; keys.el --- Emacs key bindings
;;; Commentary:
;;; Code:

(defun split-window-right-and-move-there-dammit ()
  "Split window right and move there."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-left-and-move-there-dammit ()
  "Split window left and move there."
  (interactive)
  (split-window-left)
  (windmove-left))

(defun split-window-below-and-move-there-dammit ()
  "Split window below and move there."
  (interactive)
  (split-window-below)
  (windmove-down))

;; Set Keybindings for moving text up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region transient-mark-mode active or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region transient-mark-mode active or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "C-S-l") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-S-h") 'split-window-left-and-move-there-dammit)
(global-set-key (kbd "C-S-j") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "S-<up>") 'move-text-up)
(global-set-key (kbd "S-<down>") 'move-text-down)
(global-set-key (kbd "C-<tab>") 'previous-buffer)

(use-package which-key
    :ensure t
    :config
    (which-key-mode))

;;; keys.el ends here