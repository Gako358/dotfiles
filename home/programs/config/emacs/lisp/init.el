;;; init.el --- Core Emacs configuration
;;; Commentary:
;;; Code:

(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/company.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/copilot.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/dash.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/direnv.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/editorconfig.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/eglot.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/evil.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/git.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/ivy.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/irc.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/notmuch.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/org.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/pdftool.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/project.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/sql.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/theme.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/vterm.el")
(load-file "~/Sources/dotfiles/home/programs/config/emacs/lisp/which.el")

;;GeneralSettings
(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable the scroll bar
(tab-bar-mode -1) ; Disable the tab bar
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq make-backup-files nil) ; Disable backup files
(setq auto-save-default nil) ; Disable auto save

;;Enablerelativelinenumbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;;Functiontokillallbuffersexceptthecurrentone
(defun kill-all-buffers-except-current
    ()
  "Ask for confirmation before killing all buffers except the current one."
  (interactive)
  (if
      (yes-or-no-p "Really kill all buffers except the current one? ")
      (let
          (
           (current-buffer
            (current-buffer)))
        (mapc 'kill-buffer
              (delq current-buffer
                    (buffer-list)))
        (delete-other-windows))))


;;Flycheck
(require 'flycheck)
(global-flycheck-mode) ; Enable flycheck

(defun split-window-right-and-move-there-dammit
    ()
  "Split window right and move there."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-left-and-move-there-dammit
    ()
  "Split window left and move there."
  (interactive)
  (split-window-left)
  (windmove-left))

(global-set-key
 (kbd "C-S-l") 'split-window-right-and-move-there-dammit)
(global-set-key
 (kbd "C-S-h") 'split-window-left-and-move-there-dammit)

(defun split-window-below-and-move-there-dammit
    ()
  "Split window below and move there."
  (interactive)
  (split-window-below)
  (windmove-down))

(global-set-key
 (kbd "C-S-j") 'split-window-below-and-move-there-dammit)

;;Setkeybindingsformovingbetweenwindows
(global-set-key
 (kbd "C-h") 'windmove-left)
(global-set-key
 (kbd "C-j") 'windmove-down)
(global-set-key
 (kbd "C-k") 'windmove-up)
(global-set-key
 (kbd "C-l") 'windmove-right)

;; Set keybindings for resizing windows using Shift and arrow keys
(global-set-key
 (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key
 (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key
 (kbd "C-S-<down>") 'shrink-window)
(global-set-key
 (kbd "C-S-<up>") 'enlarge-window)

;;SetKeybindingsformovingtextupanddown
(defun move-text-internal
    (arg)
  (cond
   (
    (and mark-active transient-mark-mode)
    (if
        (>
         (point)
         (mark))
        (exchange-point-and-mark))
    (let
        (
         (column
          (current-column))
         (text
          (delete-and-extract-region
           (point)
           (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark
       (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when
        (or
         (> arg 0)
         (not
          (bobp)))
      (forward-line)
      (when
          (or
           (< arg 0)
           (not
            (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down
    (arg)
  "Move region transient-mark-mode active or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up
    (arg)
  "Move region transient-mark-mode active or current line arg lines up."
  (interactive "*p")
  (move-text-internal
   (- arg)))

(global-set-key
 (kbd "S-<up>") 'move-text-up)
(global-set-key
 (kbd "S-<down>") 'move-text-down)

;; Switch between buffers
(global-set-key
 (kbd "C-<tab>") 'previous-buffer)

;; Improve performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;; init.el ends here
