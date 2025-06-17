{ osConfig
, config
, lib
, pkgs
, ...
}:
let
  inherit (osConfig.environment) desktop;

  # VS Code only tools
  vscodeOnlyTools = with pkgs; [
    # Language servers and formatters
    nil
    metals
    nixpkgs-fmt
    nodePackages.prettier
    google-java-format
    black
    rustfmt
    haskell-language-server
    kotlin-language-server

    # JavaScript/TypeScript ecosystem
    nodejs_20
    nodePackages.typescript
    typescript-language-server
    vue-language-server

    # Utilities
    jq
  ];

  # Create a PATH string for these tools
  vscodeOnlyPath = "${pkgs.lib.makeBinPath vscodeOnlyTools}";

  # Create a PATH string for system tools
  systemToolsPath = "/run/current-system/sw/bin";

  # Use the nix-profile path for Home Manager packages
  homeManagerPath = "/etc/profiles/per-user/${config.home.username}/bin";
in
{
  config = lib.mkIf (desktop.enable && desktop.develop) {
    programs.vscode = {
      enable = true;
      package = pkgs.vscode.fhs;
      mutableExtensionsDir = true;
      profiles.default = {
        extensions = with pkgs.vscode-extensions; [
          # Copilot
          github.copilot
          github.copilot-chat

          # Editor
          editorconfig.editorconfig
          ms-azuretools.vscode-docker
          ms-vscode-remote.remote-ssh
          ms-vscode-remote.remote-ssh-edit
          ms-vscode-remote.remote-containers
          ms-vscode.makefile-tools
          mkhl.direnv
          vscodevim.vim
          bmalehorn.vscode-fish

          # Formatters
          esbenp.prettier-vscode

          # Haskell
          haskell.haskell
          justusadam.language-haskell

          # Java
          redhat.java
          vscjava.vscode-java-debug
          vscjava.vscode-java-dependency
          vscjava.vscode-java-pack

          # Javascript/CSS
          vue.volar
          bradlc.vscode-tailwindcss

          # Kotlin
          mathiasfrohlich.kotlin

          # Nix
          bbenoist.nix
          jnoortheen.nix-ide

          # Python
          ms-python.python
          ms-pyright.pyright

          # Rust
          rust-lang.rust-analyzer
          tamasfe.even-better-toml

          # Scala/Metals
          scalameta.metals
          scala-lang.scala

          # Yaml/Markdown
          bierner.github-markdown-preview
          bierner.markdown-checkbox
          bierner.markdown-emoji
          bierner.markdown-footnotes
          bierner.markdown-mermaid
          bierner.markdown-preview-github-styles
        ];
        userSettings = {
          # Performance improvements for Scala/Metals
          "files.watcherExclude" = {
            "**/.bloop" = true;
            "**/.metals" = true;
            "**/.ammonite" = true;
          };

          # Git improvements
          "git.autofetch" = true;
          "git.confirmSync" = false;
          "git.enableSmartCommit" = true;

          # Editor improvements
          "workbench.tree.indent" = 20;
          "workbench.startupEditor" = "none";
          "editor.formatOnSave" = true;
          "editor.formatOnPaste" = true;
          "editor.minimap.enabled" = false;
          "editor.defaultFormatter" = "esbenp.prettier-vscode";
          "editor.lineNumbers" = "relative";

          # Code lens for better navigation
          "java.referencesCodeLens.enabled" = true;
          "java.implementationsCodeLens.enabled" = true;
          "typescript.implementationsCodeLens.enabled" = true;
          "typescript.referencesCodeLens.enabled" = true;
          "typescript.referencesCodeLens.showOnAllFunctions" = true;

          # File type associations
          "files.associations" = {
            "*.kt" = "gradle-kotlin-dsl";
            "*.css" = "tailwindcss";
          };

          # Auto-import improvements
          "javascript.updateImportsOnFileMove.enabled" = "always";
          "typescript.updateImportsOnFileMove.enabled" = "always";

          # UX improvements
          "explorer.confirmDelete" = false;
          "explorer.confirmDragAndDrop" = false;
          "diffEditor.ignoreTrimWhitespace" = false;
          "security.workspace.trust.untrustedFiles" = "open";

          # Metals configuration - let it use environment JAVA_HOME
          "metals.sbtScript" = "${pkgs.sbt}/bin/sbt";
          "metals.javaHome" = null;
          "metals.customRepositories" = [ ];
          "metals.bloopSbtLocation" = "${pkgs.bloop}/bin/bloop";
          "metals.scalafixConfigPath" = ".scalafix.conf";
          "metals.scalafixOnCompile" = false;
          "metals.scalafixConfig" = ''
            rules = [
              OrganizeImports
            ]

            OrganizeImports {
              targetDialect = Scala3
              removeUnused = true
              groupedImports = Merge
              groups = [
                "re:javax?\\."
                "scala."
                "re:^(?!scala\\.).*"
              ]
            }
          '';
          "metals.serverProperties" = [
            "-Dmetals.client=vscode"
            "-Xmx2G"
            "-Xms2G"
            "-XX:MaxMetaspaceSize=512m"
            "-Dscalafix.timeout=30s"
          ];

          # Nix Language Server
          "nix.enableLanguageServer" = true;
          "nix.serverPath" = "${pkgs.nil}/bin/nil";
          "nil.formatting.command" = [ "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" ];
          "nix.serverSettings" = {
            "nil" = {
              "formatting" = {
                "command" = [ "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt" ];
              };
            };
          };

          # Formatters
          "prettier.prettierPath" = "${pkgs.nodePackages.prettier}/bin/prettier";
          "nix.formatterPath" = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";

          # Java extension configuration to use environment variables
          "java.configuration.detectJdksAtStart" = true;
          "java.configuration.runtimes" = [ ];
          "java.import.gradle.java.home" = null;
          "java.import.maven.java.home" = null;
          "java.format.settings.url" = "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml";
          "java.format.settings.profile" = "GoogleStyle";

          # Docker
          "docker.dockerPath" = "${pkgs.docker}/bin/docker";

          # Remote SSH
          "remote.SSH.path" = "${pkgs.openssh}/bin/ssh";
          "remote.SSH.configFile" = "~/.ssh/config";

          # Markdown configuration
          "markdown.preview.fontSize" = 14;
          "markdown.preview.lineHeight" = 1.6;

          # Vue/JavaScript configuration
          "vue.server.petiteVue.supportHtmlFile" = true;
          "typescript.preferences.includePackageJsonAutoImports" = "auto";
          "javascript.preferences.includePackageJsonAutoImports" = "auto";

          # EditorConfig
          "editorconfig.generateAuto" = false;

          # TailwindCSS configuration
          "tailwindCSS.includeLanguages" = {
            "html" = "html";
            "javascript" = "javascript";
            "typescript" = "typescript";
            "vue" = "vue";
            "scala" = "html";
          };
          "tailwindCSS.experimental.classRegex" = [
            "class:\\s*?[\"'`]([^\"'`]*.*?)[\"'`]"
            "className:\\s*?[\"'`]([^\"'`]*.*?)[\"'`]"
          ];

          # Formatter configuration
          "[css]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[html]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[java]" = {
            "editor.defaultFormatter" = "redhat.java";
          };
          "[javascript]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[json]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[jsonc]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[kotlin]" = {
            "editor.defaultFormatter" = "mathiasfrohlich.kotlin";
          };
          "[markdown]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[nix]" = {
            "editor.defaultFormatter" = "jnoortheen.nix-ide";
          };
          "[python]" = {
            "editor.defaultFormatter" = "ms-python.black-formatter";
          };
          "[rust]" = {
            "editor.defaultFormatter" = "rust-lang.rust-analyzer";
          };
          "[scala]" = {
            "editor.defaultFormatter" = "scalameta.metals";
          };
          "[toml]" = {
            "editor.defaultFormatter" = "tamasfe.even-better-toml";
          };
          "[typescript]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[typescriptreact]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "[vue]" = {
            "editor.defaultFormatter" = "vue.volar";
          };
          "[yaml]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };

          "python.formatting.provider" = "black";
          "python.formatting.blackPath" = "${pkgs.black}/bin/black";

          "rust-analyzer.rustfmt.extraArgs" = [ "+nightly" ];
        };
      };
    };

    home.packages = [
      (pkgs.writeShellScriptBin "code-wrapped" ''
        # Add our specific tools to the front of the PATH but preserve the rest
        export PATH="${vscodeOnlyPath}:${systemToolsPath}:${homeManagerPath}:$PATH"
        exec ${pkgs.vscode.fhs}/bin/code "$@"
      '')
    ];

    programs.fish.shellAliases = {
      code = "code-wrapped";
    };

    xdg.desktopEntries."code" = {
      name = "Visual Studio Code";
      comment = "Code Editing. Redefined.";
      genericName = "Text Editor";
      exec = "code-wrapped %F";
      icon = "code";
      startupNotify = true;
      categories = [ "Utility" "TextEditor" "Development" "IDE" ];
      mimeType = [ "text/plain" "inode/directory" ];
      actions = {
        new-empty-window = {
          exec = "code-wrapped --new-window %F";
          name = "New Empty Window";
        };
      };
    };

    home.persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/Code"
        ".config/copilot-chat"
        ".config/github-copilot"
      ];
    };
  };
}
