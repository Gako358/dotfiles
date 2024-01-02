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

(defun +notmuch/update
    ()
  "Sync notmuch emails with server."
  (interactive)
  (let
      (
       (compilation-buffer-name-function
        (lambda
          (_)
          (format "*notmuch update*"))))
    (with-current-buffer
        (compile
         (+notmuch-get-sync-command))
      (add-hook
       'compilation-finish-functions
       (lambda
         (buf status)
         (if
             (equal status "finished\n")
             (progn
               (delete-windows-on buf)
               (bury-buffer buf)
               (notmuch-refresh-all-buffers)
               (message "Notmuch sync successful"))
           (user-error "Failed to sync notmuch data")))
       nil
       'local))))

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
        (:name "Personal Inbox" :query "tag:inbox and folder:personal" :key "p" :search-type tree)
        (:name "Work Inbox" :query "tag:inbox and folder:work" :key "w" :search-type tree)
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
