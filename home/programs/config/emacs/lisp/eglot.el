;;; eglot.el --- Language Server Protocol
;;; Commentary:
;;; Code:

;; Eglot
(use-package eglot
  :ensure t
  :config
  (setq eglot-autoshutdown t))

(use-package ccls
  :config
  (setq ccls-executable "/run/current-system/sw/bin/ccls"
    ccls-initialization-options '(:cache (:directory "/tmp/ccls-cache"))
    ccls-sem-highlight-method 'font-lock
    ccls-args '("--log-file=/tmp/ccls.log")
    ccls-extra-init-params '(:completion (:detailedLabel t))
    ccls-cache-dir "/tmp/ccls-cache")
  :hook ((c-mode c++-mode) . eglot-ensure))

(use-package eglot-java
  :config
  :hook (java-mode . eglot-java-mode)
  :mode ("\\.java\\'" . java-mode))

(use-package js2-mode
  :config
  (setq js-indent-level 2
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil)
  :hook (js2-mode . eglot-ensure)
  :mode "\\.js\\'")

(use-package nix-mode
  :hook (nix-mode . eglot-ensure)
  :mode "\\.nix\\'")

(use-package web-mode
  :config
  (dolist (pattern '("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.liquid\\'" "\\.djhtml\\'" "\\.html?\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'web-mode)))
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
  :after eglot
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
  :mode "\\.rs\\'")

;;; eglot.el ends here