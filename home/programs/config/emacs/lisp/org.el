;;; org.el --- Org Mode Configuration
;;; Commentary:
;;; Code:

;; Org Agenda
(defun +org-init-org-directory
    ()
  "Set `org-directory' to `~/Documents/org/'."
  (unless org-directory
    (setq-default org-directory "~/Documents/org/"))
  (unless org-id-locations-file
    (setq org-id-locations-file
          (expand-file-name ".orgids" org-directory))))

(defun +org-init-agenda-h
    ()
  "Configures the UI for `org-agenda'."
  (unless org-agenda-files
    (setq-default org-agenda-files
                  (list org-directory)))
  (setq-default
   org-agenda-deadline-faces
   '
   (
    (1.001 . error)
    (1.0 . org-warning)
    (0.5 . org-upcoming-deadline)
    (0.0 . org-upcoming-distant-deadline))
   org-agenda-window-setup'current-window
   org-agenda-skip-unavailable-filest
   org-agenda-span10
   org-agenda-start-on-weekdaynil
   org-agenda-start-day"-3d"
   org-agenda-inhibit-startupt))


(defun +org-init-appearance-h
    ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator" → "
        org-enforce-todo-dependenciest
        org-entities-user
        '
        (
         ("flat"  "\\flat" nil "" "" "266D" "♭")
         ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headlinet
        org-fontify-quote-and-verse-blockst
        org-fontify-whole-heading-linet
        org-hide-leading-starst
        org-image-actual-widthnil
        org-imenu-depth6
        org-priority-faces
        '
        (
         (?A . error)
         (?B . warning)
         (?C . success))
        org-startup-indentedt
        org-tags-column0
        org-use-sub-superscripts'{}
        org-startup-foldednil))

;; Org Roam
(require 'org-roam)
(setq org-roam-v2-ack t)
(setq org-roam-directory "~/Documents/Notes")
(setq org-roam-db-location "~/Documents/Notes/org-roam.db")
(setq org-roam-completion-everywhere t)

;; Org Roam UI
(require 'org-roam-ui)
(setq org-roam-ui-sync-theme t)
(setq org-roam-ui-follow t)
(setq org-roam-ui-update-on-save t)
(setq org-roam-ui-open-on-start t)

;; Org Present
(require 'org-present)
(add-hook 'org-present-mode-hook
          (lambda
            ()
            (org-present-big)
            (org-display-inline-images)
            (org-present-hide-cursor)
            (org-present-read-only)))
(add-hook 'org-present-mode-quit-hook
          (lambda
            ()
            (org-present-small)
            (org-remove-inline-images)
            (org-present-show-cursor)
            (org-present-read-write)))

;; Org Pomodoro
(require 'org-pomodoro)
(setq org-pomodoro-length 25)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 15)
(setq org-pomodoro-manual-break t)

;;; org.el ends here
