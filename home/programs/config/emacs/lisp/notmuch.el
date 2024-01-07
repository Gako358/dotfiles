;;; notmuch.el --- Notmuch configuration
;;; Commentary:
;;; Code:

(use-package notmuch
  :init
  (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)
  :config
  (evil-leader/set-key-for-mode 'notmuch-mode
    "u" '+notmuch/update
    "d" '+notmuch/search-delete)

  (setq notmuch-message-delete-tags '("-inbox" "-unread" "+archived"))

  (defun +notmuch/search-message-delete (go-next)
    "Delete message and go to next message if GO-NEXT is non-nil."
    (notmuch-search-tag notmuch-message-deleted-tags)
    (if (eq 'up go-next)
        (notmuch-search-previous-thread)
      (notmuch-search-next-thread)))

  (defun +notmuch/search-message-delete-down ()
    "Delete message and go to next message."
    (interactive)
    (+notmuch/search-message-delete 'down))

  ;; add a keybinding for deleting messages in search mode
  (define-key notmuch-search-mode-map "d" '+notmuch/search-message-delete-down)

  (defun +notmuch/open ()
    "Open notmuch."
    (interactive)
    (shell-command "notmuch new")
    (notmuch))

  (global-set-key (kbd "C-S-m") '+notmuch/open)

  ;; Run notmuch new every 5 minutes and notify if there are new emails
  (defun +notmuch/notify ()
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

  (setq notmuch-multipart/alternative-discouraged '("text/x-amp-html" "text/plain" "text/html"))
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-hello-thousands-separator ",")
  (setq notmuch-archive-tags (list "-inbox" "+archived"))
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-saved-searches
        '((:name "Personal Inbox" :query "tag:inbox and tag:personal" :key "p" :search-type tree)
          (:name "Work Inbox" :query "tag:inbox and tag:work" :key "w" :search-type tree)
          (:name "Flagged" :query "tag:flagged" :key "f" :search-type tree)
          (:name "Drafts" :query "tag:draft" :key "d")
          (:name "Recent" :query "date:7d.." :key "r" :search-type tree)
          (:name "All Mail" :query "*" :key "a" :search-type tree)))

  (setq notmuch-tagging-keys
        '(("a" notmuch-archive-tags "Archive")
          ("u" notmuch-show-mark-read-tags "Mark read")
          ("f" ("+flagged") "Flag")
          ("s" ("+spam" "-inbox") "Mark as spam")
          ("d" ("+deleted" "-inbox") "Delete")
          ("w" ("-inbox" "+waiting_for") "Waiting For"))))

;;; notmuch.el ends here