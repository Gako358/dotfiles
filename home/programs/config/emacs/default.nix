{ pkgs, ... }:
let
  metals = pkgs.metals.overrideAttrs (
    final: prev: {
      version = "1.4.1";
      deps = pkgs.stdenv.mkDerivation {
        name = "${prev.pname}-deps-1.4.1";
        buildCommand = ''
          export COURSIER_CACHE=$(pwd)
          ${pkgs.pkgs.coursier}/bin/cs fetch org.scalameta:metals_2.13:1.4.1 \
            -r bintray:scalacenter/releases \
            -r sonatype:snapshots > deps
          mkdir -p $out/share/java
          cp $(< deps) $out/share/java/
        '';
        outputHashMode = "recursive";
        outputHashAlgo = "sha256";
        outputHash = "sha256-CVAPjeTYuv0w57EK/IldJcGz8mTQnyCGAjaUf6La2rU";
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
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
    extraPackages = epkgs:
      with epkgs; [
        # Evil
        evil                # Extensible vi layer for Emacs
        evil-collection     # A set of keybindings for evil-mode
        evil-commentary     # Comment stuff out
        evil-leader         # A set of keybindings for evil-mode
        evil-matchit        # Matchit for evil-mode
        evil-org            # Org-mode keybindings for evil-mode
        evil-snipe          # Snipe text objects
        evil-surround       # Surround text objects with punctuation
        evil-visualstar     # Start a * or # search from the visual selection

        # Completion
        company             # Modular text completion framework
        company-box         # A company front-end with icons
        company-quickhelp   # Documentation popup for Company
        consult             # Consulting completing-read
        copilot             # An Emacs package for pair programming
	copilot-chat        # An Emacs copilot chat framework
        ivy                 # A generic completion mechanism
        ivy-posframe        # Display ivy in a posframe
        ivy-prescient       # Better sorting and filtering for ivy
        ivy-rich            # More friendly display transformer for ivy
        wgrep               # Writable grep buffer and apply the changes to files
        yasnippet           # Template system for Emacs

        # General
        editorconfig        # EditorConfig Emacs Plugin
        general             # Provides a more convenient way to define keybindings
        ligature            # Ligature support for Emacs

        # Org
        org                 # For keeping notes, maintaining TODO lists, and project planning
        org-drill           # A spaced repetition system for Emacs
        org-modern          # A modern org-mode distribution
        org-pomodoro        # Pomodoro technique implementation
        org-present         # A simple org-mode presentation tool
        org-roam            # A note-taking tool based on the principles of networked thought
        org-roam-ui         # A graphical user interface for org-roam

        # Project
        counsel             # Various completion functions using Ivy
        counsel-projectile  # Ivy integration for Projectile
        eat                 # Emacs All
        projectile          # Project Interaction Library for Emacs

        # Env
        envrc               # .envrc support for Emacs

        # Git
        blamer              # Show git blame information in the fringe
        forge               # Work with Git forges from the comfort of Magit
        ghub                # Minuscule client library for the Github API
        git-gutter          # Show git diff in the fringe
        git-gutter-fringe   # Fringe version of git-gutter.el
        magit               # A Git porcelain inside Emacs

        # Formaters
        blacken             # Black formatter for Python
        nixpkgs-fmt         # Nixpkgs formatting

        # Theme
        dashboard                 # A startup screen extracted from Spacemacs
        kaolin-themes             # A low contrast color theme for Emacs
        nerd-icons                # Nerd icons for Emacs
        rainbow-delimiters        # Highlight delimiters such as parentheses, brackets or braces according to their depth
        rainbow-mode              # Colorize color names in buffers
	      all-the-icons             # A package for inserting developer icons
        all-the-icons-ivy-rich    # More friendly display transformer for ivy

        # Programming language packages.
        dockerfile-mode                     # Major mode for editing Dockerfiles
        eglot-booster                       # Eglot booster
        eldoc-box                           # Display function signatures at point
        haskell-mode                        # Haskell development environment
        jsonrpc                             # Json RPC library
        markdown-mode                       # Major mode for editing Markdown files
        nix-ts-mode                         # Nix development environment
        python-mode                         # Major mode for editing Python files
        rustic                              # Rust development environment
        scala-ts-mode                       # Scala development environment
        treesit-grammars.with-all-grammars  # Tree-sitter grammars
        web-mode                            # Major mode for editing web templates
        yaml-mode                           # Major mode for editing YAML files
      ];
    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = [
    pkgs.nixpkgs-fmt
    pkgs.emacs-lsp-booster
    metals
    pkgs.scalafmt
    pkgs.astyle
    pkgs.fd
    pkgs.nil
  ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
  };
}
