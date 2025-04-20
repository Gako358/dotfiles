{ lib
, pkgs
, config
, osConfig
, ...
}:
let
  metalsVersion = "1.5.1";
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
        outputHash = "sha256-2FA2B/WzNGU4B785pn5zZ9Xj64huzbSbr2Or+CxUMlI=";
      };
      buildInputs = [ final.deps ];
    }
  );

  eglot-booster = pkgs.emacsPackages.melpaBuild {
    pname = "eglot-booster";
    version = "20241029";

    commit = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";

    src = pkgs.fetchFromGitHub {
      owner = "jdtsmith";
      repo = "eglot-booster";
      rev = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
      hash = "sha256-PLfaXELkdX5NZcSmR1s/kgmU16ODF8bn56nfTh9g6bs=";
    };

    recipe = pkgs.writeText "recipe" ''
      (eglot-booster
      :repo "jdtsmith/eglot-booster"
      :fetcher github
      :files ("*.el"))
    '';
  };

  vue-ts-mode = pkgs.emacsPackages.melpaBuild {
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

  bivrost-theme = pkgs.emacsPackages.melpaBuild {
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

  # Embeded packages
  emacsOnlyTools = [
    metals
    pkgs.astyle
    pkgs.basedpyright
    pkgs.black
    pkgs.dtach
    pkgs.emacs-lsp-booster
    pkgs.kotlin-language-server
    pkgs.nil
    pkgs.nixpkgs-fmt
    pkgs.nodePackages.prettier
    pkgs.typescript-language-server
    pkgs.vue-language-server
  ];

  # Create a PATH string for these tools
  emacsOnlyPath = "${pkgs.lib.makeBinPath emacsOnlyTools}";

  # Create a PATH string for system tools
  systemToolsPath = "/run/current-system/sw/bin";

  # Use the nix-profile path for Home Manager packages
  homeManagerPath = "/etc/profiles/per-user/merrinx/bin";

in
{
  config = lib.mkIf osConfig.environment.desktop.enable {
    home.persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/copilot-chat"
        ".config/github-copilot"
      ];
    };

    sops.secrets = lib.mkIf osConfig.service.sops.enable {
      "forge_auth" = {
        path = "${config.home.homeDirectory}/.authinfo";
      };
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs30;
      extraPackages = epkgs:
        with epkgs; [
          # Appearande
          all-the-icons # A package for inserting developer icons
          all-the-icons-completion
          all-the-icons-ivy-rich # More friendly display transformer for ivy
          bivrost-theme # Custom theme
          dashboard # A startup screen extracted from Spacemacs
          spaceline # A mode-line theming package
          nerd-icons # Nerd icons for Emacs
          nerd-icons-completion # Nerd icons for completion
          nerd-icons-corfu # Nerd icons for corfu
          powerline #A utility library for creating a custom mode-line
          rainbow-delimiters # Highlight delimiters such as parentheses, brackets or braces according to their depth
          rainbow-mode # Colorize color names in buffers

          # Completion
          cape # Completion At Point Extensions.
          copilot # AI code completion.
          copilot-chat # Chat with Copilot.
          corfu # Completion Overlay Region Function.
          embark # Context-sensitive actions.
          embark-consult # Consult preview using embark
          flycheck # On-the-fly syntax checking
          marginalia # Annotations for completion candidates.
          orderless # Space-separated matching components.
          vertico # Vertical interactive completion UI.
          vertico-posframe # Vertico completion UI with posframe.

          # Evil
          evil # Extensible vi layer for Emacs
          evil-collection # A set of keybindings for evil-mode
          evil-commentary # Comment stuff out
          evil-leader # A set of keybindings for evil-mode
          evil-matchit # Matchit for evil-mode
          evil-org # Org-mode keybindings for evil-mode
          evil-snipe # Snipe text objects
          evil-surround # Surround text objects with punctuation
          evil-visualstar # Start a * or # search from the visual selection
          evil-numbers # Increment and decrement numbers in Emacs

          # Edit
          apheleia # A universal formatter interface

          # Filetree
          dirvish # Directory viewer for Emacs

          # General
          alert # Growl-like notifications
          dash # A modern list library for Emacs
          editorconfig # EditorConfig Emacs Plugin
          envrc # .envrc support for Emacs
          f # A modern API for working with files and directories in Emacs
          fringe-helper # Helper functions for fringe bitmaps
          general # Provides a more convenient way to define keybindings
          gntp # Growl Notification Transport Protocol
          goto-chg # Goto the point of the most recent edit
          ligature # Ligature support for Emacs
          log4e # Logging framework for Emacs
          s # The long lost Emacs string manipulation library
          password-store # Emacs interface for pass, the standard Unix password manager
          ripgrep #Ripgrep for Emacs
          wgrep # Writable grep buffer.

          # grammars
          treesit-grammars.with-all-grammars # Tree-sitter grammars

          # Programming language packages.
          eglot-java # Java development environment
          haskell-ts-mode # Haskell development environment
          kotlin-ts-mode # Kotlin development environment
          markdown-mode # Major mode for editing Markdown files
          nix-ts-mode # Major mode for editing Nix files
          scala-ts-mode # Scala development environment
          sql-indent # Indentation for SQL files
          tide # TypeScript Interactive Development Environment
          web-mode # Major mode for editing web templates
          vue-ts-mode # Major mode for editing Vue3 files
          yaml-pro # Major mode for editing YAML files

          # LSP
          eglot-booster # Eglot booster
          eldoc-box # Display function signatures at point

          # Mail
          mu4e # Emacs mail client
          mu4e-alert # Emacs notification daemon for mu4e

          # Navigation
          consult # Consulting completing-read
          consult-projectile # Consult interface for Projectile

          # Org
          org # For keeping notes, maintaining TODO lists, and project planning
          org-drill # A spaced repetition system for Emacs
          org-msg # A msg system used to compose emails for Emacs
          org-modern # A modern org-mode distribution
          org-pomodoro # Pomodoro technique implementation
          org-present # A simple org-mode presentation tool
          org-roam # A note-taking tool based on the principles of networked thought
          org-roam-ui # A graphical user interface for org-roam

          # Project
          projectile # Project Interaction Library for Emacs
          projectile-ripgrep # Ripgrep integration for Projectile

          # SSH
          tramp # Transparently access remote machines

          # Terminal
          detached # Detached mode for Emacs
          eat # Emacs Anywhere Terminal

          # Version
          forge # Work with github forges
          git-gutter # Show git diff in the fringe
          git-gutter-fringe # Fringe version of git-gutter.el
          magit # A Git porcelain inside Emacs
          vundo # Undo tree visualizer
        ];
      extraConfig = builtins.readFile ./init.el;
    };

    home.file = {
      ".emacs.d" = {
        source = ./.;
        recursive = true;
      };
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
        Environment = [ "PATH=${emacsOnlyPath}:${systemToolsPath}:${homeManagerPath}:$PATH" ];
      };
    };

    # Create a wrapper for emacsclient that adds tools to PATH
    home.packages = [
      (pkgs.writeShellScriptBin "ec" ''
        # Add our specific tools to the front of the PATH but preserve the rest
        export PATH="${emacsOnlyPath}:${systemToolsPath}:${homeManagerPath}:$PATH"
        exec ${pkgs.emacs30}/bin/emacsclient "$@"
      '')
    ];
  };
}
