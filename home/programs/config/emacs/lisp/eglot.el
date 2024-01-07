;;; eglot.el --- Language Server Protocol
;;; Commentary:
;;; Code:

(use-package eglot
  :config
  (evil-leader/set-key
    "lf" 'eglot-format-buffer
    "lr" 'eglot-rename
    "lgd" 'xref-find-definitions
    "lgr" 'xref-find-references
    "lga" 'eglot-code-actions
    "lgt" 'eglot-find-typeDefinition
    "lgh" 'eglot-inlay-hints-mode
    "lgo" 'eglot-code-action-organize-imports
    "lgq" 'eglot-code-action-quickfix))

(use-package ccls
  :config
  (setq ccls-executable "/run/current-system/sw/bin/ccls")
  (setq ccls-initialization-options '(:cache (:directory "/tmp/ccls-cache")))
  (setq ccls-sem-highlight-method 'font-lock)
  (setq ccls-args '("--log-file=/tmp/ccls.log"))
  (setq ccls-extra-init-params '(:completion (:detailedLabel t)))
  (setq ccls-cache-dir "/tmp/ccls-cache")
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

(use-package docker
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package eglot-java
  :config
  (add-hook 'java-mode-hook #'eglot-java-mode)
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
  (evil-leader/set-key-for-mode 'java-mode
    "n" 'eglot-java-project-new
    "b" 'eglot-java-project-build-refresh
    "f" 'eglot-java-file-new
    "m" 'eglot-java-run-main
    "t" 'eglot-java-run-test
    "ul" 'eglot-java-upgrade-lsp-server
    "uj" 'eglot-java-upgrade-junit-jar))

(use-package js2-mode
  :config
  (setq js-indent-level 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Nix
(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook #'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

;; Web-mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-part-padding 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-code-indent-offset 2))

;; Python
(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'eglot-ensure))

(use-package blacken)

;; SQL
(use-package sql
  :config
  (add-hook 'sql-mode-hook #'eglot-ensure))

;; Scala
(use-package scala-mode
  :config
  (add-hook 'scala-mode-hook #'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode)))

;; SBT
(use-package sbt-mode
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (add-to-list 'auto-mode-alist '("\\.s\\(cala\\|bt\\)$" . scala-mode)))

;; TailwindCSS
(use-package css-mode
  :config
  (add-hook 'css-mode-hook #'eglot-ensure))

;; Typescript
(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook #'eglot-ensure)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; Vue
(use-package vue-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook #'eglot-ensure))

;; XML
(use-package nxml-mode
  :config
  (add-hook 'nxml-mode-hook #'setup-xml))

(use-package sql
  :ensure t
  :config
  (setq sql-mysql-login-params
        '((user :default "root")
          (server :default "localhost")
          (port :default 3306))))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rustic
(use-package rustic
  :config
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  (evil-leader/set-key-for-mode 'rustic-mode
    "b" 'rustic-cargo-build
    "c" 'rustic-cargo-check
    "r" 'rustic-cargo-run
    "t" 'rustic-cargo-test))

;;; eglot.el ends here
