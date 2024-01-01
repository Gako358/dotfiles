{ pkgs, config, ... }:
let
  # Emacs Copilot Installation Definition
  emacsCopilotSrc = builtins.fetchGit {
    url = "https://github.com/zerolfx/copilot.el.git";
    rev = "421703f5dd5218ec2a3aa23ddf09d5f13e5014c2";
  };

in
{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs:
      with epkgs; [
        # Core packages
        evil # Extensible Vi Layer for Emacs
        evil-collection # A set of keybindings for evil mode
        evil-nerd-commenter # Comment/uncomment lines efficiently
        evil-surround # Emulates vim-surround functionality in Emacs
        evil-leader # A minor mode for Emacs that emulates Vim's "leader" key
        general # Provides a more convenient way to define keybindings
        which-key # Displays available keybindings in popup

        # Optional packages.
        all-the-icons # A package for inserting developer icons
        all-the-icons-dired # Shows icons for each file in dired mode
        all-the-icons-ivy-rich # More friendly display transformer for ivy
        counsel # Various completion functions using Ivy
        dired-single # Reuse the dired buffer
        direnv # Environment switcher for Emacs
        docker # Docker integration
        dockerfile-mode # Major mode for editing Dockerfiles
        editorconfig # EditorConfig Emacs Plugin
        elfeed # An extensible web feed reader
        eldoc # Show function arglist or variable docstring in echo area
        emojify # Display emojis in Emacs
        eshell-prompt-extras # Display extra information and color for your eshell prompt
        flycheck # On-the-fly syntax checking
        ghub # Minuscule client library for the Github API
        hydra # Make bindings that stick around
        ivy # A generic completion mechanism
        ivy-posframe # Display ivy in a posframe
        ivy-prescient # Better sorting and filtering for ivy
        ivy-rich # More friendly display transformer for ivy
        ligature # Ligature support for Emacs
        magit # A Git porcelain inside Emacs
        nerd-icons # Nerd icons for Emacs
        notmuch # A fast mail indexer and mail reader
        ob-http # HTTP request in org-mode
        org # For keeping notes, maintaining TODO lists, and project planning
        org-drill # A spaced repetition system for Emacs
        org-pomodoro # Pomodoro technique implementation
        org-present # A simple org-mode presentation tool
        org-roam # A note-taking tool based on the principles of networked thought
        org-roam-ui # A graphical user interface for org-roam
        ox-hugo # Org exporter to Hugo
        password-store # Password manager
        pdf-tools # Emacs support library for PDF files
        pretty-mode # Redisplay parts of the buffer as pretty symbols
        projectile # Project Interaction Library for Emacs
        protobuf-mode # Major mode for editing protocol buffers
        simple-httpd # A simple HTTP server
        vterm # Fully-featured terminal emulator
        websocket # WebSocket client and server
        web-mode # Major mode for editing web templates
        writegood-mode # A minor mode to aid in finding common writing problems

        # Theme
        doom-modeline # A fancy and fast mode-line
        doom-themes # An opinionated pack of modern color-themes

        # Language Server
        dap-mode # Debug Adapter Protocol mode
        ccls # C/C++/ObjC language server
        lsp-java # Java
        lsp-pyright # Python language server
        lsp-metals # Scala language server
        lsp-treemacs # Treemacs integration f
        lsp-tailwindcss # Tailwind CSS support for lsp-mode
        lsp-mode # An implementation of the Language Server Protocol
        lsp-ui # UI integrations for lsp-mode

        # Programming language packages.
        blacken # Black formatter for Python
        company # Modular text completion framework
        company-box # A company front-end with icons
        json-mode # Major mode for editing JSON files
        js2-mode # Improved JavaScript editing mode
        markdown-mode # Major mode for editing Markdown files
        nix-mode # Nix integration
        python-mode # Major mode for editing Python files
        rustic # Rust development environment
        sbt-mode # Major mode for editing SBT files
        scala-mode # Major mode for editing Scala files
        tide # TypeScript Interactive Development Environment
        typescript-mode # Major mode for editing TypeScript files
        vue-mode # Major mode for editing Vue.js files
        yaml-mode # Major mode for editing YAML files
        yasnippet # Template system for Emacs
      ];
    extraConfig = ''
      ;; General Settings
      (setq inhibit-startup-message t) ; Disable startup message
      (menu-bar-mode -1) ; Disable the menu bar
      (tool-bar-mode -1) ; Disable the toolbar
      (scroll-bar-mode -1) ; Disable the scroll bar
      (tab-bar-mode -1) ; Disable the tab bar
      (setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

      ;; Enable relative line numbers
      (setq display-line-numbers-type 'relative)
      (global-display-line-numbers-mode t)

      ;; Package Specific Settings

      ;; Yas Snippets
      (yas-global-mode 1) ; Enable YASnippet

      ;; Writegood Mode
      (require 'writegood-mode)
      (add-hook 'text-mode-hook 'writegood-mode)

      ;; Function to kill all buffers except the current one
      (defun kill-all-buffers-except-current ()
        "Ask for confirmation before killing all buffers except the current one."
        (interactive)
        (if (yes-or-no-p "Really kill all buffers except the current one? ")
            (let ((current-buffer (current-buffer)))
              (mapc 'kill-buffer (delq current-buffer (buffer-list)))
              (delete-other-windows))))

      ;; Evil Mode
      (require 'evil)
      (evil-mode 1)

      ;; Set the leader key to space
      (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
        "b" 'ivy-switch-buffer
        "f" 'counsel-find-file
        "F" 'counsel-git-grep
        "m" 'notmuch
        "k" 'kill-buffer
        "q" 'kill-all-buffers-except-current
        "w" 'save-buffer
        "x" 'counsel-M-x
        "d" 'dired
        "t" 'vterm
        "p" 'projectile-command-map
        "/" 'magit-status
        "gc" 'comment-or-uncomment-region

        ;; Org Keybindings
        "oa" 'org-agenda
        "os" 'org-schedule
        "od" 'org-deadline
        "of" 'org-agenda-file-to-front
        "ot" 'org-insert-todo-heading
        "orl" 'org-roam-buffer-toggle
        "orf" 'org-roam-node-find
        "ori" 'org-roam-node-insert
        "ors" 'org-roam-db-sync
        "orr" 'org-roam-ui-mode
        "opl" 'org-present
        "opq" 'org-present-quit

        ;; LSP Keybindings
        "lws" 'lsp-ui-sideline-mode
        "lwd" 'lsp-ui-doc-mode
        "lwr" 'lsp-ui-peek-find-references
        "lwf" 'lsp-ui-peek-find-definitions
        "lwa" 'lsp-ui-peek-find-implementation
        "lwh" 'lsp-ui-doc-glance
        "lwe" 'lsp-treemacs-errors-list
        "lf" 'lsp-format-buffer
        "lpf" 'blacken-buffer
        "lxf" 'xml-pretty-print
        "lr" 'lsp-rename
        "lR" 'lsp-workspace-restart
        "la" 'lsp-execute-code-action
        "ld" 'lsp-describe-thing-at-point
        "lgt" 'lsp-goto-type-definition
        "lgi" 'lsp-goto-implementation
        "lsh" 'lsp-symbol-highlight
        "lwa" 'lsp-workspace-folders-add
        "lwr" 'lsp-workspace-folders-remove
        "lws" 'lsp-workspace-folders-switch

        ;; Java Keybindings
        "jo" 'lsp-java-organize-imports
        "jbp" 'lsp-java-build-project
        "jupc" 'lsp-java-update-project-configuration
        "jan" 'lsp-java-actionable-notifications
        "juus" 'lsp-java-update-user-settings
        "jus" 'lsp-java-update-server
        "jgt" 'lsp-java-generate-to-string
        "jgeh" 'lsp-java-generate-equals-and-hash-code
        "jgo" 'lsp-java-generate-overrides
        "jggs" 'lsp-java-generate-getters-and-setters
        "jth" 'lsp-java-type-hierarchy
        "jec" 'lsp-java-extract-to-constant
        "jaum" 'lsp-java-add-unimplemented-methods
        "jcp" 'lsp-java-create-parameter
        "jcf" 'lsp-java-create-field
        "jcl" 'lsp-java-create-local
        "jem" 'lsp-java-extract-method
        "jai" 'lsp-java-add-import
        "jtb" 'lsp-jt-browser
        "jro" 'lsp-jt-report-open
        "jlm" 'lsp-jt-lens-mode

        ;; DAP Java Keybindings
        "jdd" 'dap-java-debug
        "jrtm" 'dap-java-run-test-method
        "jdtm" 'dap-java-debug-test-method
        "jrtc" 'dap-java-run-test-class
        "jdtc" 'dap-java-debug-test-class)

      (evil-leader/set-key-for-mode 'notmuch-mode
        "u" '+notmuch/update
        "d" '+notmuch/search-delete)

      ;; Flycheck
      (require 'flycheck)
      (global-flycheck-mode) ; Enable flycheck

      ;; Set keybindings for moving between windows
      (global-set-key (kbd "C-h") 'windmove-left)
      (global-set-key (kbd "C-j") 'windmove-down)
      (global-set-key (kbd "C-k") 'windmove-up)
      (global-set-key (kbd "C-l") 'windmove-right)

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
        "Move region (transient-mark-mode active) or current line arg lines down."
        (interactive "*p")
        (move-text-internal arg))

      (defun move-text-up (arg)
        "Move region (transient-mark-mode active) or current line arg lines up."
        (interactive "*p")
        (move-text-internal (- arg)))

      (global-set-key (kbd "S-<up>") 'move-text-up)
      (global-set-key (kbd "S-<down>") 'move-text-down)

      ;; Notmuch Configuration
      (require 'notmuch)
      (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)

      (defvar +notmuch-delete-tags '("+trash" "-inbox" "-unread"))
        "Tags applied to mark emails for deletion."

      (defun +notmuch/search-delete ()
        "Mark all selected emails for deletion."
        (interactive)
        (notmuch-search-tag +notmuch-delete-tags)
        (notmuch-tree-next-message))

      (defun +notmuch/update ()
        "Sync notmuch emails with server."
        (interactive)
        (let ((compilation-buffer-name-function (lambda (_) (format "*notmuch update*"))))
          (with-current-buffer (compile (+notmuch-get-sync-command))
          (add-hook
            'compilation-finish-functions
            (lambda (buf status)
              (if (equal status "finished\n")
                  (progn
                    (delete-windows-on buf)
                    (bury-buffer buf)
                    (notmuch-refresh-all-buffers)
                    (message "Notmuch sync successful"))
                (user-error "Failed to sync notmuch data")))
            nil
            'local))))

      (setq notmuch-multipart/alternative-discouraged '("text/x-amp-html" "text/plain" "text/html"))
      (setq notmuch-search-oldest-first nil)
      (setq notmuch-hello-thousands-separator ",")
      (setq notmuch-archive-tags (list "-inbox" "+archived"))
      (setq notmuch-mua-cite-function (quote message-cite-original-without-signature))
      (setq notmuch-saved-searches
        (quote
          ((:name "Personal Inbox" :query "tag:inbox and folder:prvAcc" :key "p" :search-type tree)
            (:name "Work Inbox" :query "tag:inbox and folder:gitAcc" :key "w" :search-type tree)
            (:name "Flagged" :query "tag:flagged" :key "f" :search-type tree)
            (:name "Drafts" :query "tag:draft" :key "d")
            (:name "Recent" :query "date:7d.." :key "r" :search-type tree)
            (:name "All Mail" :query "*" :key "a" :search-type tree))))

      (setq notmuch-tagging-keys
        (quote
          (("a" notmuch-archive-tags "Archive")
            ("u" notmuch-show-mark-read-tags "Mark read")
            ("f" ("+flagged") "Flag")
            ("s" ("+spam" "-inbox") "Mark as spam")
            ("d" ("+deleted" "-inbox") "Delete")
            ("w" ("-inbox" "+waiting_for") "Waiting For"))))

      ;; Sending mail with MSMTP
      (setq sendmail-program "msmtp"
        message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        message-directory "~/Documents/mails"
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t)

      ;; Which Key
      (require 'which-key)
      (which-key-mode)

      ;; Ivy & Counsel
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "(%d/%d) ")

      ;; Org Agenda
      (defun +org-init-org-directory ()
        (unless org-directory
          (setq-default org-directory "~/Documents/org/"))
        (unless org-id-locations-file
          (setq org-id-locations-file (expand-file-name ".orgids" org-directory))))

      (defun +org-init-agenda-h ()
        (unless org-agenda-files
          (setq-default org-agenda-files (list org-directory)))
        (setq-default
         org-agenda-deadline-faces
         '((1.001 . error)
           (1.0 . org-warning)
           (0.5 . org-upcoming-deadline)
           (0.0 . org-upcoming-distant-deadline))
         org-agenda-window-setup 'current-window
         org-agenda-skip-unavailable-files t
         org-agenda-span 10
         org-agenda-start-on-weekday nil
         org-agenda-start-day "-3d"
         org-agenda-inhibit-startup t))


      (defun +org-init-appearance-h ()
        "Configures the UI for `org-mode'."
        (setq org-indirect-buffer-display 'current-window
              org-eldoc-breadcrumb-separator " → "
              org-enforce-todo-dependencies t
              org-entities-user
              '(("flat"  "\\flat" nil "" "" "266D" "♭")
                ("sharp" "\\sharp" nil "" "" "266F" "♯"))
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t
              org-fontify-whole-heading-line t
              org-hide-leading-stars t
              org-image-actual-width nil
              org-imenu-depth 6
              org-priority-faces
              '((?A . error)
                (?B . warning)
                (?C . success))
              org-startup-indented t
              org-tags-column 0
              org-use-sub-superscripts '{}
              org-startup-folded nil))

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
                  (org-present-read-write)))

      ;; Org Pomodoro
      (require 'org-pomodoro)
      (setq org-pomodoro-length 25)
      (setq org-pomodoro-short-break-length 5)
      (setq org-pomodoro-long-break-length 15)
      (setq org-pomodoro-manual-break t)

      ;; Pdf Tools
      (require 'pdf-tools)
      (pdf-tools-install)
      (setq-default pdf-view-display-size 'fit-page)
      (setq pdf-annot-activate-created-annotations t)
      (add-hook 'pdf-view-mode-hook
        (lambda ()
          (when (bound-and-true-p display-line-numbers-mode)
            (display-line-numbers-mode -1))))

      ;; Projectile
      (setq projectile-project-search-path '("~/Projects/" ("~/Projects/workspace/" . 1)))
      (projectile-mode +1)

      ;; Copilot Configuration
      (let ((copilot-dir "~/.emacs.d/emacsCopilot")
            (copilot-file "~/.emacs.d/emacsCopilot/copilot.el"))
        ;; Check if the copilot.el file exists
        (when (file-exists-p copilot-file)
          ;; Add the directory to the load-path
          (add-to-list 'load-path copilot-dir)
          ;; Try to load the copilot module and catch any errors
          (condition-case err
              (progn
                (require 'copilot)
                (add-hook 'prog-mode-hook 'copilot-mode)
                (define-key copilot-completion-map (kbd "C-p") 'copilot-accept-completion)
                (define-key copilot-mode-map (kbd "C-j") #'copilot-next-completion)
                (define-key copilot-mode-map (kbd "C-k") #'copilot-previous-completion))
            ;; If there's an error, print a message (you can also log or take other actions)
            (error (message "Failed to load copilot: %s" err)))))

      ;; Enable Dap
      (require 'dap-mode)
      (dap-mode 1)
      (dap-ui-mode 1)
      (dap-tooltip-mode 1)
      (tooltip-mode 1) ;; Use tooltips for mouse hover
      (dap-ui-controls-mode 1)

      ;; LSP Mode
      (require 'lsp-mode)
      (add-hook 'prog-mode-hook #'lsp-deferred)
      (setq lsp-headerline-breadcrumb-enable nil)

      ;; Enable LSP-UI
      (require 'lsp-ui)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)

      ;; Enable CCLS
      (require 'ccls)
      (setq ccls-executable "/run/current-system/sw/bin/ccls")
      (setq ccls-initialization-options '(:cache (:directory "/tmp/ccls-cache")))
      (setq ccls-sem-highlight-method 'font-lock)
      (setq ccls-args '("--log-file=/tmp/ccls.log"))
      (setq ccls-extra-init-params '(:completion (:detailedLabel t)))
      (setq ccls-cache-dir "/tmp/ccls-cache")
      (add-hook 'c-mode-hook #'lsp-deferred)
      (add-hook 'c++-mode-hook #'lsp-deferred)

      ;; Enable Docker
      (require 'docker)
      (require 'dockerfile-mode)
      (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

      ;; Enable Java
      (require 'lsp-java)
      (require 'dap-java)
      (add-hook 'java-mode-hook #'lsp-deferred)
      (add-hook 'java-mode-hook #'dap-mode)

      ;; Enable Javascript
      (require 'js2-mode)
      (setq js-indent-level 2)
      (setq js2-mode-show-parse-errors nil)
      (setq js2-mode-show-strict-warnings nil)
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-hook 'js2-mode-hook #'lsp-deferred)

      ;; Enable Json
      (require 'json-mode)
      (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

      ;; Enable Markdown
      (require 'markdown-mode)
      (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

      ;; Enable Nix
      (require 'nix-mode)
      (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

      ;; Web-mode configurations
      (require 'web-mode)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

      (setq web-mode-enable-auto-closing t)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-part-padding 2)
      (setq web-mode-script-padding 2)
      (setq web-mode-style-padding 2)
      (setq web-mode-code-indent-offset 2)

      ;; Enable Python
      (require 'python-mode)
      (require 'lsp-pyright)
      (require 'blacken)
      (add-hook 'python-mode-hook #'lsp-deferred)

      ;; Enable SQL
      (require 'sql)
      (add-hook 'sql-mode-hook #'lsp-deferred)

      (defun upcase-sql-keywords ()
        (interactive)
        (save-excursion
          (dolist (keywords sql-mode-postgres-font-lock-keywords)
            (goto-char (point-min))
            (while (re-search-forward (car keywords) nil t)
              (goto-char (+ 1 (match-beginning 0)))
              (when (eql font-lock-keyword-face (face-at-point))
                (backward-char)
                (upcase-word 1)
                (forward-char))))))

      ;; Enable Scala
      (defun setup-scala-mode ()
             (require 'lsp-metals)
             (add-hook 'before-save-hook 'lsp-format-buffer))

      (require 'scala-mode)
      (add-hook 'scala-mode-hook #'lsp-metals-bootstrapped)
      (add-hook 'scala-mode-hook #'setup-scala-mode)

      ;; Enable SBT
      (require 'sbt-mode)

      (setq sbt:program-options '("-Dsbt.supershell=false"))
      (add-to-list 'auto-mode-alist '("\\.s\\(cala\\|bt\\)$" . scala-mode))

      ;; Enable LSP-TailwindCSS
      (require 'lsp-tailwindcss)
      (add-hook 'css-mode-hook #'lsp-deferred)

      ;; Enable Typescript
      (require 'typescript-mode)
      (defun setup-tide-mode ()
             (interactive)
             (tide-setup)
             (setq flycheck-check-syntax-automatically '(save mode-enabled))
             (eldoc-mode +1)
             (tide-hl-identifier-mode +1))

      (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
      (flycheck-add-mode 'typescript-tslint 'typescript-mode)
      (add-hook 'typescript-mode-hook #'lsp-deferred)
      (add-hook 'typescript-mode-hook #'setup-tide-mode)

      ;; Enable Vue
      (require 'vue-mode)
      (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
      (add-hook 'vue-mode-hook #'lsp-deferred)

      ;; Xml Pretty Print
      (require 'nxml-mode)
      (defun xml-pretty-print ()
        (interactive)
        (save-excursion
          (shell-command-on-region (point-min) (point-max)
                                   "xmllint --format --encode utf-8 -"
                                   (buffer-name) t)
          (nxml-mode)
          (indent-region begin end)))

      (defun setup-xml ()
             (require 'sideshow)
             (add-to-list 'hs-special-modes-alist
                          '(nxml-mode
                            "<!--\\|<[^/>]*[^/]>"
                            "-->\\|</[^/>]*[^/]>"
                            "<!--"
                            'nxml-forward-element
                            nil))
              (hs-minor-mode 1))

      (add-hook 'nxml-mode-hook #'setup-xml)

      ;; Enable Rustic
      (require 'rustic)
      (setq rustic-lsp-server 'rust-analyzer)
      (add-hook 'rustic-mode-hook #'lsp-deferred)

      ;; Enable Completion
      (require 'company)
      (require 'company-box)
      (defun setup-company-mode ()
             (yas-minor-mode-on)
             (company-box-mode 1))

      (add-hook 'company-mode-hook 'setup-company-mode)
      (add-hook 'after-init-hook 'global-company-mode)
      (setq company-idle-delay 0.2)
      (setq company-tooltip-align-annotations t)

      ;; Direnv Configuration
      (require 'direnv)
      (direnv-mode)

      ;; EditorConfig
      (require 'editorconfig)
      (editorconfig-mode 1)

      ;; Vterm Configuration
      (require 'vterm)

      ;; Nerd Icons
      (require 'nerd-icons)
      (require 'all-the-icons)

      ;; Doom Themes
      (require 'doom-themes)
      (load-theme 'doom-one t)
      (require 'doom-modeline)
      (doom-modeline-mode 1)
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-height 35)
      (setq doom-modeline-minor-modes t)
      (setq doom-modeline-enable-word-count t)
      (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
      (setq doom-modeline-buffer-encoding t)
      (setq doom-modeline-indent-info t)
      (setq doom-modeline-total-line-number t)
      (setq doom-modeline-github t)
      (setq doom-modeline-github-interval (* 10 60))
    '';
  };

  home.packages = with pkgs; [
    # Needed LSP Packages...
    # Sick I know, should be build into emacs
    rnix-lsp
  ];

  home.file."./.emacs.d/emacsCopilot".source = emacsCopilotSrc;
  services.emacs.enable = true;
}
