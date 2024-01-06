;;; irc.el --- IRC configuration
;;; Commentary:
;;; Code:

(require 'erc)
(setq
 erc-nick "merrinx"
 erc-user-full-name "Knut Oien"
 erc-rename-buffers t
 erc-interperate-mirc-color t
 erc-hide-list '("JOIN" "PART" "QUIT")
 erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#nixos" "#java"))
 erc-quit-reason (lambda (s) (concat "I'm off to " s "!"))
 erc-modules
 '(autoaway autojoin button completion fill irccontrols keep-place
            list match menu move-to-prompt netsplit networks noncommands
            readonly ring stamp track))

(defun +erc/connect ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697 :nick erc-nick))

(defun +erc/start ()
  "Connect to IRC and join channels."
  (interactive)
  (+erc/connect))

(defun +erc/persp ()
  "Switch to IRC perspective."
  (interactive)
  (+erc/start)
  (persp-switch "irc"))

(defun +erc/quit ()
  "Quit IRC."
  (interactive)
  (erc-quit-server "I'm off to bed!")
  (persp-kill "irc")
  (persp-switch "main"))

(global-set-key (kbd "C-S-i") '+erc/persp)
(global-set-key (kbd "C-S-q") '+erc/quit)

(setq doom-modeline-irc t)
(setq doom-modeline-irc-stylize 'identity)

;;; irc.el ends here
