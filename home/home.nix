{pkgs, ...}: let
  username = "merrinx";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    any-nix-shell        # fish support for nix shell
    acpi                 # battery info
    arandr               # screen layout manager
    brightnessctl        # control screen brightness
    bottom               # alternative to htop & ytop
    cacert               # ca certificates
    dbeaver              # database manager
    dconf2nix            # dconf (gnome) files to nix converter
    docker-compose       # docker manager
    dive                 # explore docker layers
    duf                  # disk usage/free utility
    eza                  # a better `ls`
    fd                   # "find" for files
    gimp                 # gnu image manipulation program
    jump                 # fast directory navigation
    killall              # kill processes by name
    lazygit              # terminal git ui
    libsecret            # secret management
    ncdu                 # disk space info (a better du)
    nitch                # minimal system information fetch
    nix-index            # locate packages containing certain nixpkgs
    nix-output-monitor   # nom: monitor nix commands
    ouch                 # painless compression and decompression for your terminal
    pavucontrol          # pulseaudio volume control
    paprefs              # pulseaudio preferences
    prettyping           # a nicer ping
    pulsemixer           # pulseaudio mixer
    rage                 # encryption tool for secrets management
    ranger               # terminal file manager
    ripgrep              # fast grep
    scrot                # screenshot tool
    slurp                # select a region in a wayland compositor
    spotify              # music streaming
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    unzip                # unzip files
    virt-manager         # virtual machine manager
    wgetpaste            # paste to pastebin
    xarchiver            # archive manager
    zip                  # zip files
  ];
in {
  programs.home-manager.enable = true;
  imports = builtins.concatMap import [
    ./cli
    ./programs
    ./services
    ./themes
  ];
  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "23.11";
    packages = defaultPkgs;
    sessionVariables = {
      EDITOR = "nvim";
    };
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
