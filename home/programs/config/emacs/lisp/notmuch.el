;;; notmuch.el --- Notmuch configuration
;;; Commentary:
;;; Code:

;; Notmuch Configuration
(require 'notmuch)
(add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)

(defvar +notmuch-delete-tags '
  ("+trash" "-inbox" "-unread"))
"Tags applied to mark emails for deletion."

(defun +notmuch/search-delete
    ()
  "Mark all selected emails for deletion."
  (interactive)
  (notmuch-search-tag +notmuch-delete-tags)
  (notmuch-tree-next-message))

(defun +notmuch/open
    ()
  "Open notmuch."
  (interactive)
  (shell-command "notmuch new")
  (notmuch))

(global-set-key
 (kbd "C-S-m") '+notmuch/open)

;; Run notmuch new every 5 minutes and notify if there are new emails
(defun +notmuch/notify
    ()
  "Notify if there are new emails."
  (interactive)
  (shell-command "notmuch new")
  (let ((count (string-to-number (shell-command-to-string "notmuch count tag:unread"))))
    (if (> count 0)
        (notifications-notify
         :title "New Mail for you, you idiot...!"
         :body (format "You have %d unread emails." count)
         :urgency 'critical))))

(run-with-timer 0 300 '+notmuch/notify)

(setq notmuch-multipart/alternative-discouraged '
      ("text/x-amp-html" "text/plain" "text/html"))
(setq notmuch-search-oldest-first nil)
(setq notmuch-hello-thousands-separator ",")
(setq notmuch-archive-tags
      (list "-inbox" "+archived"))
(setq notmuch-mua-cite-function
      (quote message-cite-original-without-signature))
(setq notmuch-saved-searches
      (quote
       (
        (:name "Personal Inbox" :query "tag:inbox and tag:personal" :key "p" :search-type tree)
        (:name "Work Inbox" :query "tag:inbox and tag:work" :key "w" :search-type tree)
        (:name "Flagged" :query "tag:flagged" :key "f" :search-type tree)
        (:name "Drafts" :query "tag:draft" :key "d")
        (:name "Recent" :query "date:7d.." :key "r" :search-type tree)
        (:name "All Mail" :query "*" :key "a" :search-type tree))))

(setq notmuch-tagging-keys
      (quote
       (
        ("a" notmuch-archive-tags "Archive")
        ("u" notmuch-show-mark-read-tags "Mark read")
        ("f"
         ("+flagged") "Flag")
        ("s"
         ("+spam" "-inbox") "Mark as spam")
        ("d"
         ("+deleted" "-inbox") "Delete")
        ("w"
         ("-inbox" "+waiting_for") "Waiting For"))))

;; Sending mail with MSMTP
(setq sendmail-program "msmtp")
(setq message-directory "~/Documents/mails")
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-kill-buffer-on-exit t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq mail-specify-envelope-from t)

;;; notmuch.el ends here
