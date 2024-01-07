;;; org.el --- Org Mode Configuration
;;; Commentary:
;;; Code:

(use-package org
  :init
  (setq org-directory (or org-directory "~/Documents/org/")
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-agenda-files (list org-directory)
        org-agenda-deadline-faces '((1.001 . error)
                                    (1.0 . org-warning)
                                    (0.5 . org-upcoming-deadline)
                                    (0.0 . org-upcoming-distant-deadline))
        org-agenda-window-setup 'current-window
        org-agenda-skip-unavailable-files t
        org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-inhibit-startup t
        org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭")
                            ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        org-startup-folded nil))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t
        org-roam-directory "~/Documents/Notes"
        org-roam-db-location "~/Documents/Notes/org-roam.db"
        org-roam-completion-everywhere t))

(use-package org-roam-ui
  :init
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-present
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package org-pomodoro
  :init
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15
        org-pomodoro-manual-break t))

(evil-leader/set-key
  "oa" 'org-agenda
  "os" 'org-schedule
  "od" 'org-deadline
  "oi" 'org-insert-structure-template
  "of" 'org-agenda-file-to-front
  "ot" 'org-insert-todo-heading
  "orl" 'org-roam-buffer-toggle
  "orf" 'org-roam-node-find
  "ori" 'org-roam-node-insert
  "ors" 'org-roam-db-sync
  "orr" 'org-roam-ui-mode
  "opl" 'org-present
  "opq" 'org-present-quit)

;;; org.el ends here