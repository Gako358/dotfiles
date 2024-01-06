;;; eglot.el --- Language Server Protocol
;;; Commentary:
;;; Code:

;; Enable Eglot
(require 'eglot)

(evil-leader/set-key
  "lf" 'eglot-format-buffer
  "lr" 'eglot-rename
  "lgd" 'xref-find-definitions
  "lgr" 'xref-find-references
  "lga" 'eglot-code-actions
  "lgt" 'eglot-find-typeDefinition
  "lgh" 'eglot-inlay-hints-mode
  "lgo" 'eglot-code-action-organize-imports
  "lgq" 'eglot-code-action-quickfix)

;; Enable CCLS
(require 'ccls)
(setq ccls-executable "/run/current-system/sw/bin/ccls")
(setq ccls-initialization-options '
      (:cache
       (:directory "/tmp/ccls-cache")))
(setq ccls-sem-highlight-method 'font-lock)
(setq ccls-args '
      ("--log-file=/tmp/ccls.log"))
(setq ccls-extra-init-params '
      (:completion
       (:detailedLabel t)))
(setq ccls-cache-dir "/tmp/ccls-cache")
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

;; Enable Docker
(require 'docker)
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '
             ("Dockerfile\\'" . dockerfile-mode))

;; Enable Java
(require 'eglot-java)
(add-hook 'java-mode-hook #'eglot-java-mode)
(add-to-list 'auto-mode-alist '
             ("\\.java\\'" . java-mode))

(evil-leader/set-key-for-mode 'java-mode
  "n" 'eglot-java-project-new
  "b" 'eglot-java-project-build-refresh
  "f" 'eglot-java-file-new
  "m" 'eglot-java-run-main
  "t" 'eglot-java-run-test
  "ul" 'eglot-java-upgrade-lsp-server
  "uj" 'eglot-java-upgrade-junit-jar)

;; Enable Javascript
(require 'js2-mode)
(setq js-indent-level 2)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(add-hook 'js2-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '
             ("\\.js\\'" . js2-mode))

;; Enable Json
(require 'json-mode)
(add-to-list 'auto-mode-alist '
             ("\\.json\\'" . json-mode))

;; Enable Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '
             ("\\.md\\'" . markdown-mode))

;; Enable Nix
(require 'nix-mode)
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '
             ("\\.nix\\'" . nix-mode))

;; Web-mode configurations
(require 'web-mode)
(add-to-list 'auto-mode-alist '
             ("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '
             ("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-closing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-part-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-code-indent-offset 2)

;; Enable Python
(require 'python-mode)
(require 'blacken)
(add-hook 'python-mode-hook #'eglot-ensure)

;; Enable SQL
(require 'sql)
(add-hook 'sql-mode-hook #'eglot-ensure)

(defun upcase-sql-keywords
    ()
  "Upcase SQL keywords."
  (interactive)
  (save-excursion
    (dolist
        (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char
       (point-min))
      (while
          (re-search-forward
           (car keywords) nil t)
        (goto-char
         (+ 1
            (match-beginning 0)))
        (when
            (eql font-lock-keyword-face
                 (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

;; Enable Scala
(require 'scala-mode)
(add-hook 'scala-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '
             ("\\.scala\\'" . scala-mode))

;; Enable SBT
(require 'sbt-mode)
(setq sbt:program-options '
      ("-Dsbt.supershell=false"))
(add-to-list 'auto-mode-alist '
             ("\\.s\\(cala\\|bt\\)$" . scala-mode))

;; Enable TailwindCSS
(add-hook 'css-mode-hook #'eglot-ensure)

;; Enable Typescript
(require 'typescript-mode)
(defun setup-tide-mode
    ()
  "Setup Tide mode."
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '
        (save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(add-to-list 'auto-mode-alist '
             ("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '
             ("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Enable Vue
(require 'vue-mode)
(add-to-list 'auto-mode-alist '
             ("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook #'eglot-ensure)

;; Xml Pretty Print
(require 'nxml-mode)
(defun xml-pretty-print
    ()
  "Pretty print XML."
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point-min)
     (point-max)
     "xmllint --format --encode utf-8 -"
     (buffer-name) t)
    (nxml-mode)
    (indent-region begin end)))

(defun setup-xml
    ()
  "Setup XML."
  (require 'sideshow)
  (add-to-list 'hs-special-modes-alist
               '
               (nxml-mode
                "<!--\\|<[^/>]*[^/]>"
                "-->\\|</[^/>]*[^/]>"
                "<!--"
                'nxml-forward-element
                nil))
  (hs-minor-mode 1))

(add-hook 'nxml-mode-hook #'setup-xml)

;; Enable Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable Rustic
(require 'rustic)
(add-hook 'rustic-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '
             ("\\.rs\\'" . rustic-mode))

(evil-leader/set-key-for-mode 'rustic-mode
  "b" 'rustic-cargo-build
  "c" 'rustic-cargo-check
  "r" 'rustic-cargo-run
  "t" 'rustic-cargo-test)

;; Enable Whitespace
(require 'whitespace)
(require 'whitespace-cleanup-mode)

(evil-leader/set-key
  ;;WhitespaceKeybindings
  "ws" 'whitespace-mode
  "wc" 'whitespace-cleanup-mode)

;;; eglot.el ends here
