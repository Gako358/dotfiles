let
  more = { pkgs, ... }: {
    home.packages = with pkgs; [
      # Common packages
      any-nix-shell # fish support for nix shell
      acpi # battery info
      arandr # screen layout manager
      brightnessctl # control screen brightness
      bottom # alternative to htop & ytop
      cacert # ca certificates
      dconf2nix # dconf (gnome) files to nix converter
      dive # explore docker layers
      duf # disk usage/free utility
      eza # a better `ls`
      fd # "find" for files
      jump # fast directory navigation
      killall # kill processes by name
      libsecret # secret management
      mediainfo # media information
      ncdu # disk space info (a better du)
      nitch # minimal system information fetch
      nix-index # locate packages containing certain nixpkgs
      nix-output-monitor # nom: monitor nix commands
      nix-prefetch-git # prefetch git sources
      ouch # painless compression and decompression for your terminal
      prettyping # a nicer ping
      rage # encryption tool for secrets management
      ranger # file manager
      ripgrep # fast grep
      tldr # summary of a man page
      tree # display files in a tree view
      unzip # unzip files
      xarchiver # archive manager
      zip # zip files
    ];

    programs = {
      bat.enable = true;

      broot = {
        enable = true;
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fzf = {
        enable = true;
        defaultCommand = "fd --type file --follow"; # FZF_DEFAULT_COMMAND
        defaultOptions = [ "--height 20%" ]; # FZF_DEFAULT_OPTS
        fileWidgetCommand = "fd --type file --follow"; # FZF_CTRL_T_COMMAND
      };

      gpg.enable = true;
      jq.enable = true;

      ssh.enable = true;
    };
  };
in
[
  ./browser/chromium.nix
  ./browser/discord.nix
  ./config
  ./alacritty.nix
  ./git.nix
  ./neovim.nix
  ./network.nix
  more
]
