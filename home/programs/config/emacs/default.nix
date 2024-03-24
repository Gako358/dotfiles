{ pkgs, ... }:
let
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
        general # Provides a more convenient way to define keybindings
        which-key # Displays available keybindings in popup
        all-the-icons # A package for inserting developer icons
        all-the-icons-dired # Shows icons for each file in dired mode
        all-the-icons-ivy-rich # More friendly display transformer for ivy
        blamer # Show git blame information in the fringe
        company # Modular text completion framework
        company-box # A company front-end with icons
        company-quickhelp # Documentation popup for Company
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
        erc # An IRC client for Emacs
        evil # Extensible vi layer for Emacs
        evil-org # Org-mode keybindings for evil-mode
        evil-collection # A set of keybindings for evil-mode
        evil-commentary # Comment stuff out
        evil-leader # A set of keybindings for evil-mode
        evil-surround # Surround text objects with punctuation
        evil-visualstar # Start a * or # search from the visual selection
        evil-matchit # Matchit for evil-mode
        evil-snipe # Snipe text objects
        flycheck # On-the-fly syntax checking
        forge # Work with Git forges from the comfort of Magit
        ghub # Minuscule client library for the Github API
        git-gutter # Show git diff in the fringe
        git-gutter-fringe # Fringe version of git-gutter.el
        highlight-thing # Highlight the current line, the current symbol, and more
        ivy # A generic completion mechanism
        ivy-posframe # Display ivy in a posframe
        ivy-prescient # Better sorting and filtering for ivy
        ivy-rich # More friendly display transformer for ivy
        ligature # Ligature support for Emacs
        magit # A Git porcelain inside Emacs
        nerd-icons # Nerd icons for Emacs
        nixpkgs-fmt # Nixpkgs formatting
        org # For keeping notes, maintaining TODO lists, and project planning
        org-drill # A spaced repetition system for Emacs
        org-modern # A modern org-mode distribution
        org-pomodoro # Pomodoro technique implementation
        org-present # A simple org-mode presentation tool
        org-roam # A note-taking tool based on the principles of networked thought
        org-roam-ui # A graphical user interface for org-roam
        pdf-tools # Emacs support library for PDF files
        projectile # Project Interaction Library for Emacs
        rainbow-delimiters # Highlight delimiters such as parentheses, brackets or braces according to their depth
        rainbow-mode # Colorize color names in buffers
        simple-httpd # A simple HTTP server
        treemacs # A tree layout file explorer
        treemacs-tab-bar # A tab bar for treemacs
        treemacs-projectile # Integration between treemacs and projectile
        treemacs-evil # Evil keybindings for treemacs
        treemacs-icons-dired # Show icons for each file in dired mode
        treemacs-magit # Magit integration for treemacs
        treemacs-persp # Perspective integration for treemacs
        tree-sitter # Incremental parsing system for Emacs
        tree-sitter-langs # Tree-sitter grammar for various languages
        vterm # Fully-featured terminal emulator
        multi-vterm # Multiple vterm buffers
        web-mode # Major mode for editing web templates
        whitespace-cleanup-mode # Intelligently call whitespace-cleanup on save

        # Theme
        kaolin-themes # A low contrast color theme for Emacs

        # Programming language packages.
        blacken # Black formatter for Python
        eglot-java # Java support for eglot
        nix-mode # Nix integration
        python-mode # Major mode for editing Python files
        rustic # Rust development environment
        sbt-mode # Major mode for editing SBT files
        scala-mode # Major mode for editing Scala files
      ];
    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = with pkgs; [
    nixpkgs-fmt
    nil
  ];

  home.file."./.emacs.d/emacsCopilot".source = emacsCopilotSrc;
  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
  };
}
