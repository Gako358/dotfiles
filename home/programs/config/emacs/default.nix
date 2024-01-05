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
        blamer # Show git blame information in the fringe
        counsel # Various completion functions using Ivy
        counsel-projectile # Ivy integration for Projectile
        dashboard # A startup screen extracted from Spacemacs
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
        git-gutter # Show git diff in the fringe
        git-gutter-fringe # Fringe version of git-gutter.el
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
        org-modern # A modern org-mode distribution
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

        # Theme
        doom-modeline # A fancy and fast mode-line
        doom-themes # An opinionated pack of modern color-themes

        # Programming language packages.
        blacken # Black formatter for Python
        ccls # C/C++/ObjC language server
        company # Modular text completion framework
        company-box # A company front-end with icons
        company-quickhelp # Documentation popup for Company
        eglot-java # Java support for eglot
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
      ];
    extraConfig = builtins.readFile ./lisp/init.el;
  };

  home.packages = with pkgs; [
    # Needed LSP Packages...
    # Sick I know, should be build into emacs
    rnix-lsp
  ];

  home.file."./.emacs.d/emacsCopilot".source = emacsCopilotSrc;
  services.emacs.enable = true;
}
