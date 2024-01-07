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
  :hook ((c-mode c++-mode) . eglot-ensure))

(use-package eglot-java
  :config
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
  (evil-leader/set-key-for-mode 'java-mode
    "n" 'eglot-java-project-new
    "b" 'eglot-java-project-build-refresh
    "f" 'eglot-java-file-new
    "m" 'eglot-java-run-main
    "t" 'eglot-java-run-test
    "ul" 'eglot-java-upgrade-lsp-server
    "uj" 'eglot-java-upgrade-junit-jar)
  :hook (java-mode . eglot-java-mode))

(use-package js2-mode
  :config
  (setq js-indent-level 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :hook (js2-mode . eglot-ensure)
  :mode "\\.js\\'")

(use-package nix-mode
  :hook (nix-mode . eglot-ensure)
  :mode "\\.nix\\'")

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
  :hook (python-mode . eglot-ensure))

(use-package blacken)

;; SQL
(use-package sql
  :ensure t
  :config
  (setq sql-mysql-login-params
        '((user :default "root")
          (server :default "localhost")
          (port :default 3306)))
  :hook (sql-mode . eglot-ensure))

;; Scala
(use-package scala-mode
  :hook (scala-mode . eglot-ensure)
  :mode "\\.scala\\'")

;; SBT
(use-package sbt-mode
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :mode "\\.s\\(cala\\|bt\\)$")

;; TailwindCSS
(use-package css-mode
  :hook (css-mode . eglot-ensure))

;; Typescript
(use-package typescript-mode
  :hook ((typescript-mode . eglot-ensure)
         (typescript-mode . setup-tide-mode))
  :mode ("\\.ts\\'" "\\.tsx\\'"))

;; Vue
(use-package vue-mode
  :hook (vue-mode . eglot-ensure)
  :mode "\\.vue\\'")

;; XML
(use-package nxml-mode
  :hook (nxml-mode . setup-xml))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rustic
(use-package rustic
  :hook (rustic-mode . eglot-ensure)
  :config
  (evil-leader/set-key-for-mode 'rustic-mode
    "b" 'rustic-cargo-build
    "c" 'rustic-cargo-check
    "r" 'rustic-cargo-run
    "t" 'rustic-cargo-test)
  :mode "\\.rs\\'")

;;; eglot.el ends here
