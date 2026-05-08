_: {
  flake.homeModules.programs-emacs =
    {
      lib,
      pkgs,
      config,
      osConfig,
      ...
    }:
    let
      metalsVersion = "2.0.0-M10";
      metals = pkgs.metals.overrideAttrs (
        final: prev: {
          deps = pkgs.stdenv.mkDerivation {
            name = "${prev.pname}-deps-${metalsVersion}";
            buildCommand = ''
              export COURSIER_CACHE=$(pwd)
              ${pkgs.pkgs.coursier}/bin/cs fetch org.scalameta:metals_2.13:${metalsVersion} \
                -r bintray:scalacenter/releases \
                -r sonatype:snapshots > deps
              mkdir -p $out/share/java
              cp $(< deps) $out/share/java/
            '';
            outputHashMode = "recursive";
            outputHashAlgo = "sha256";
            outputHash = "sha256-e9wfYbxAlfBKst67WA2zbmeyYmGHvLbfxHjMs+BC+N4=";
          };
          buildInputs = [ final.deps ];
        }
      );

      haskell-ts-mode-custom = pkgs.emacs.pkgs.melpaBuild {
        pname = "haskell-ts-mode";
        version = "1";
        commit = "625b8c5d4c907f822c74c951bfe1bbdd8b187d4e";

        src = pkgs.fetchgit {
          url = "https://codeberg.org/pranshu/haskell-ts-mode.git";
          rev = "625b8c5d4c907f822c74c951bfe1bbdd8b187d4e";
          sha256 = "sha256-G3vKgJAE0kRtwWxsqJGdDOeYpYxUszv0e1fZEiUZuUI=";
        };

        recipe = pkgs.writeText "recipe" ''
          (haskell-ts-mode :fetcher git :url "https://codeberg.org/pranshu/haskell-ts-mode.git")
        '';
      };

      ben-custom = pkgs.emacs.pkgs.melpaBuild {
        pname = "ben";
        version = "0.12.12";
        commit = "c91cce703deb0cafb9f344cbc66d63791665fcb2";

        src = pkgs.fetchgit {
          url = "https://codeberg.org/pastor/ben.el.git";
          rev = "c91cce703deb0cafb9f344cbc66d63791665fcb2";
          sha256 = "sha256-p7KjEYEzBEk4vrUIaeMFezZkOJdhTuWiFt/y5ptOmhY=";
        };

        packageRequires = with pkgs.emacs.pkgs; [ inheritenv ];

        recipe = pkgs.writeText "recipe" ''
          (ben :fetcher git :url "https://codeberg.org/pastor/ben.el.git")
        '';
      };

      pg-el-custom = pkgs.emacs.pkgs.melpaBuild {
        pname = "pg";
        version = "0.65";
        commit = "0eed71bf642c40bac7937a6e8602f41917c90505";
        src = pkgs.fetchFromGitHub {
          owner = "emarsden";
          repo = "pg-el";
          rev = "0eed71bf642c40bac7937a6e8602f41917c90505";
          hash = "sha256-209BJhz1D8g/pJUsc9E+BjlWQAHbeeiT/FhwuO7ytnI=";
        };
        packageRequires = [ pkgs.emacs.pkgs.peg ];
        recipe = pkgs.writeText "recipe" ''
          (pg
           :repo "emarsden/pg-el"
           :fetcher github
           :files ("pg.el" "pg-geometry.el" "pg-gis.el" "pg-bm25.el" "pg-lo.el"))
        '';
      };

      pgmacs-custom = pkgs.emacs.pkgs.melpaBuild {
        pname = "pgmacs";
        version = "0.30";
        commit = "b27999b6b2676514dae6c879e7a72a2beca58a39";
        src = pkgs.fetchFromGitHub {
          owner = "emarsden";
          repo = "pgmacs";
          rev = "b27999b6b2676514dae6c879e7a72a2beca58a39";
          hash = "sha256-cM69R2kz65h8G9hnqDpETD0A/zIbxZ1kK6+gA+V7bhE=";
        };
        packageRequires = [ pg-el-custom ];
        recipe = pkgs.writeText "recipe" ''
          (pgmacs
           :repo "emarsden/pgmacs"
           :fetcher github
           :files ("*.el"))
        '';
      };

      vue-ts-mode = pkgs.emacs.pkgs.melpaBuild {
        pname = "vue-ts-mode";
        version = "20231029";
        commit = "5ec5bb317b80ce394e156c61b7b9c63996382a68";
        src = pkgs.fetchFromGitHub {
          owner = "8uff3r";
          repo = "vue-ts-mode";
          rev = "5ec5bb317b80ce394e156c61b7b9c63996382a68";
          hash = "sha256-1SOlRcq0KSO9n+isUSL5IhlujD4FWcU5I0zP6xuInuQ=";
        };
        recipe = pkgs.writeText "recipe" ''
          (vue-ts-mode
          :repo "8uff3r/vue-ts-mode"
          :fetcher github
          :files ("*.el"))
        '';
      };

      bivrost-theme = pkgs.emacs.pkgs.melpaBuild {
        pname = "bivrost-theme";
        version = "20250330";
        commit = "38c6fae362356800b7391fdb4aa999fd76fa1d24";
        src = pkgs.fetchFromGitHub {
          owner = "gako358";
          repo = "bivrost";
          rev = "38c6fae362356800b7391fdb4aa999fd76fa1d24";
          hash = "sha256-bsVoX9G8RfEOAhJHKnCJhYlaCjGA4R75fTPwgN+GGx4=";
        };
        recipe = pkgs.writeText "recipe" ''
          (bivrost-theme
           :repo "gako358/bivrost"
           :fetcher github
           :files ("*.el"))
        '';
      };

      hunspellWithDicts = pkgs.hunspell.withDicts (dicts: [
        dicts.en_GB-ise
        dicts.nb_NO
      ]);

      treesit-predicate-rewrite = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/mwolson/emacs-shared/55d07fa51e28627ff8feb77e5d53cf122eda3c96/init/treesit-predicate-rewrite.el";
        hash = "sha256-2JHHTYVqZcBkldlxqhSZftaZd8jhC+k5Ew3YIKMoJew=";
      };

      # Embedded packages
      emacsOnlyTools = [
        hunspellWithDicts
        metals
        pkgs.astyle
        pkgs.black
        pkgs.dtach
        pkgs.gemini-cli
        pkgs.gh
        pkgs.kotlin-language-server
        pkgs.nil
        pkgs.nixfmt
        pkgs.nixfmt-tree
        pkgs.prettier
        pkgs.python313Packages.python-lsp-server
        pkgs.nodejs
        pkgs.postgresql_17
        pkgs.tailwindcss-language-server
        pkgs.typescript
        pkgs.typescript-language-server
        pkgs.vue-language-server
      ];

      # Create a PATH string for these tools
      emacsOnlyPath = "${pkgs.lib.makeBinPath emacsOnlyTools}";

      # Create a PATH string for system tools
      systemToolsPath = "/run/current-system/sw/bin";

      # Add wrappers bin for sudo and other setup programs
      wrappersPath = "/run/wrappers/bin";

      # Use the nix-profile path for Home Manager packages
      homeManagerPath = "/etc/profiles/per-user/${config.home.username}/bin";

      inherit (osConfig.environment) desktop;
    in
    {
      config = lib.mkIf (desktop.enable && desktop.develop) {
        sops = lib.mkIf osConfig.service.sops.enable {
          secrets = {
            "forge_auth" = { };
            "pr_auth" = { };
          };

          templates."authinfo" = {
            path = "${config.home.homeDirectory}/.authinfo";
            content = ''
              ${config.sops.placeholder."forge_auth"}
              ${config.sops.placeholder."pr_auth"}
            '';
          };
        };

        programs.emacs = {
          enable = true;
          package = pkgs.emacs30;
          extraPackages =
            epkgs: with epkgs; [
              # Appearance
              bivrost-theme # Custom theme
              dashboard # A startup screen extracted from Spacemacs
              indent-bars # Visualise indentation with vertical bars
              spaceline # A mode-line teeming package
              nerd-icons # Nerd icons for Emacs
              nerd-icons-completion # Nerd icons for completion
              nerd-icons-corfu # Nerd icons for corfu
              powerline # A utility library for creating a custom mode-line

              # Completion
              cape # Completion At Point Extensions.
              corfu # Completion Overlay Region Function.
              copilot-chat # Github copilot-chat extension.
              eca # Editor Code Assistant, AI-powered agentic
              embark # Context-sensitive actions.
              embark-consult # Consult preview using embark
              flycheck # On-the-fly syntax checking
              marginalia # Annotations for completion candidates.
              orderless # Space-separated matching components.
              vertico # Vertical interactive completion UI.
              vertico-posframe # Vertigo completion UI with posframe.

              # Documentation
              pdf-tools # Document viewer

              # Evil
              evil # Extensible vi layer for Emacs
              evil-collection # A set of keybindings for evil-mode
              evil-leader # A set of keybindings for evil-mode
              evil-mc # Multiple cursors for evil-mode
              evil-surround # Surround text objects with punctuation
              evil-visualstar # Start a * or # search from the visual selection
              evil-numbers # Increment and decrement numbers in Emacs

              # Edit
              apheleia # A universal formatted interface
              dtrt-indent # Auto-detect buffer indentation offset/tabs

              # File tree
              dirvish # Directory viewer for Emacs

              # General
              alert # Growl-like notifications (mu4e-alert dep)
              dash # A modern list library for Emacs
              editorconfig # EditorConfig Emacs Plugin
              ben-custom # Asynchronous .envrc/direnv support (fork of envrc)
              f # A modern API for working with files and directories in Emacs
              fringe-helper # Helper functions for fringe bitmaps
              gntp # Growl Notification Transport Protocol (alert dep)
              goto-chg # Goto the point of the most recent edit (evil dep)
              log4e # Logging framework for Emacs
              s # The long lost Emacs string manipulation library
              wgrep # Writable grep buffer.

              # Tree-sitter support - specify only the grammars needed
              (treesit-grammars.with-grammars (
                grammars: with grammars; [
                  tree-sitter-bash
                  tree-sitter-c
                  tree-sitter-css
                  tree-sitter-dockerfile
                  tree-sitter-haskell
                  tree-sitter-java
                  tree-sitter-javascript
                  tree-sitter-kotlin
                  tree-sitter-markdown
                  tree-sitter-nix
                  tree-sitter-python
                  tree-sitter-rust
                  tree-sitter-scala
                  tree-sitter-tsx
                  tree-sitter-typescript
                  tree-sitter-vue
                  tree-sitter-yaml
                ]
              ))

              # Programming language packages.
              haskell-ts-mode-custom # Haskell development environment
              kotlin-ts-mode # Kotlin development environment
              markdown-mode # Major mode for editing Markdown files
              nix-ts-mode # Major mode for editing Nix files
              scala-ts-mode # Scala development environment
              sql-indent # Indentation for SQL files
              web-mode # Major mode for editing web templates
              vue-ts-mode # Major mode for editing Vue3 files
              yaml-pro # Major mode for editing YAML files

              # Database
              peg # PEG parser library (required by pg.el)
              pg-el-custom # Pure-elisp PostgreSQL wire-protocol client
              pgmacs-custom # PostgreSQL browser/editor

              # LSP
              lsp-mode # LSP client
              lsp-ui # UI enhancements for lsp-mode
              lsp-java # Java support (replaces eglot-java)
              lsp-metals # Scala Metals support
              lsp-haskell # Haskell LSP support
              lsp-tailwindcss # Tailwind CSS LSP add-on
              dap-mode # Debug Adapter Protocol client (dap-java now ships with lsp-java; Scala DAP is driven by lsp-metals)
              eldoc-box # Display function signatures at point

              # Mail
              mu4e # Emacs mail client
              mu4e-alert # Emacs notification daemon for mu4e

              # Navigation
              consult # Consulting completing-read
              consult-gh # Search GitHub via gh CLI through consult
              consult-lsp # Consult LSP for diagnostics
              consult-projectile # Consult interface for Projectile
              dumb-jump # Heuristic jump-to-definition via grep/ripgrep (xref backend)

              # Org
              org # For keeping notes, maintaining TODO lists, and project planning
              org-msg # A msg system used to compose emails for Emacs
              org-modern # A modern org-mode distribution
              org-roam # A note-taking tool based on the principles of networked thought

              # Project
              projectile # Project Interaction Library for Emacs

              # SSH
              tramp # Transparently access remote machines

              # Terminal
              detached # Detached mode for Emacs
              vterm # Emacs vterm integration
              vterm-toggle # Toggle vterm

              # Version
              diff-hl # Highlight uncommitted changes in the fringe/margin
              forge # Work with github forges
              magit # A Git porcelain inside Emacs
              pr-review # Review GitHub/GitLab PRs in Emacs
              vundo # Undo tree visualiser
            ];
          extraConfig = ''
            ;; Workaround for Emacs bug#79687 (libtree-sitter >= 0.26 rejects
            ;; the predicate names that Emacs 30.2 emits). Must be loaded
            ;; before any tree-sitter font-lock query is compiled.
            (load "${treesit-predicate-rewrite}" nil t)

            ${builtins.readFile ./init.el}
            (setq lsp-typescript-tsdk "${pkgs.typescript}/lib/node_modules/typescript/lib")
            (setq lsp-clients-typescript-plugins
                  (vector
                   (list :name "@vue/typescript-plugin"
                         :location "${pkgs.vue-language-server}/lib/language-tools/packages/typescript-plugin"
                         :languages (vector "vue"))))
          '';
        };

        # Set up the Emacs service
        services.emacs = {
          enable = true;
          client.enable = true;
          defaultEditor = true;
          socketActivation.enable = true;
        };

        # Use a wrapper script for the Emacs service
        systemd.user.services.emacs = {
          Service = {
            Environment = [
              "PATH=${emacsOnlyPath}:${wrappersPath}:${systemToolsPath}:${homeManagerPath}:$PATH"
            ];
          };
        };

        home = {
          file = {
            ".emacs.d" = {
              source = ./.;
              recursive = true;
            };
          };
          persistence."/persist/" = {
            directories = [
              ".cache/eca"
              ".config/github-copilot"
            ];
          };
          # Create a wrapper for emacsclient that adds tools to PATH
          packages = [
            (pkgs.writeShellScriptBin "ec" ''
              # Add our specific tools to the front of the PATH but preserve the rest
              export PATH="${emacsOnlyPath}:${wrappersPath}:${systemToolsPath}:${homeManagerPath}:$PATH"
              exec ${pkgs.emacs30}/bin/emacsclient "$@"
            '')
          ];
        };
      };
    };
}
