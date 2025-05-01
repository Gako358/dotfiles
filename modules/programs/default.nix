let
  more = { pkgs, ... }: {
    home.packages = with pkgs; [
      acpi # battery info
      arandr # screen layout manager
      brightnessctl # control screen brightness
      bottom # alternative to htop & ytop
      cacert # ca certificates
      dconf2nix # dconf (gnome) files to nix converter
      ffmpegthumbnailer # thumbnailer for video files
      headsetcontrol # control logitech headsets
      imagemagick # image manipulation
      killall # kill processes by name
      libsecret # secret management
      mediainfo # media information
      nix-index # locate packages containing certain nixpkgs
      nix-output-monitor # nom: monitor nix commands
      nix-prefetch-git # prefetch git sources
      ouch # painless compression and decompression for your terminal
      paprefs # pulseaudio preferences
      pavucontrol # pulseaudio volume control
      prettyping # a nicer ping
      poppler # pdf tools
      rage # encryption tool for secrets management
      ranger # file manager
      ripgrep # fast grep
      tldr # summary of a man page
      tree # display files in a tree view
      unzip # unzip files
      xarchiver # archive manager
      zip # zip files
    ];
  };
in
[
  ./emacs
  ./hyprland
  ./neofetch
  ./wofi
  ./alacritty.nix
  ./bat.nix
  ./broot.nix
  ./direnv.nix
  ./discord.nix
  ./fish.nix
  ./fzf.nix
  ./gimp.nix
  ./git.nix
  ./hyprlock.nix
  ./jq.nix
  ./neovim.nix
  ./pcmanfm.nix
  ./repl.nix
  ./slack.nix
  ./spotify.nix
  ./starship.nix
  ./waybar.nix
  ./wine.nix
  ./zen.nix
  more
]
