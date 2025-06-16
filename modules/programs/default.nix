let
  more = { pkgs, ... }: {
    home.packages = with pkgs; [
      acpi # battery info
      brightnessctl # control screen brightness
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
  ./icue.nix
  ./jq.nix
  ./neovim.nix
  ./pcmanfm.nix
  ./repl.nix
  ./slack.nix
  ./spotify.nix
  ./starship.nix
  ./vscode.nix
  ./waybar.nix
  ./wine.nix
  ./wowup.nix
  ./zen.nix
  more
]
