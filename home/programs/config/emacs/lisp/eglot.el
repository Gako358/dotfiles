;;; eglot.el --- Language Server Protocol
;;; Commentary:
;;; Code:

;; Eglot
(use-package eglot
  :ensure t
  :config
  (setq eglot-autoshutdown t))

(use-package eglot-java
  :hook (java-mode . eglot-java-mode)
  :mode ("\\.java\\'" . java-mode))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure)
  :mode "\\.nix\\'")

;; Python and blacken formatting
(use-package blacken)
(use-package python-mode
  :hook (python-mode . eglot-ensure))

;; Rustic
(use-package rustic
  :after eglot
  :hook (rustic-mode . eglot-ensure)
  :mode "\\.rs\\'")

;; SBT
(use-package sbt-mode
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :mode "\\.s\\(cala\\|bt\\)$")

;; Scala
(use-package scala-mode
  :hook (scala-mode . eglot-ensure)
  :mode "\\.scala\\'")

;; SQL
(use-package sql
  :after eglot
  :hook (sql-mode . eglot-ensure)
  :mode "\\.sql\\'")

;; web-mode setup
(defun vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
                    "lib"
                    (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t)))))

(add-to-list 'eglot-server-programs
             `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

(add-to-list 'eglot-server-programs
             `(typescript-mode . ("typescript-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

(add-to-list 'eglot-server-programs
             `(js-mode . ("typescript-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

(add-to-list 'eglot-server-programs
             `(tailwindcss-mode . ("tailwindcss-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

;; XML Configuration
(use-package nxml-mode
  :hook (nxml-mode . eglot-ensure)
  :mode "\\.xml\\'")

;;; eglot.el ends here
