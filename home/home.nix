{pkgs, ...}: let
  username = "merrinx";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    any-nix-shell        # fish support for nix shell
    acpi                 # battery info
    arandr               # screen layout manager
    asciinema            # record the terminal
    brightnessctl        # control screen brightness
    bottom               # alternative to htop & ytop
    cacert               # ca certificates
    cowsay               # cowsay fortune teller with random images
    dbeaver              # database manager
    dconf2nix            # dconf (gnome) files to nix converter
    docker-compose       # docker manager
    dive                 # explore docker layers
    drawio               # diagram design
    duf                  # disk usage/free utility
    eza                  # a better `ls`
    fd                   # "find" for files
    geany                # text editor
    gimp                 # gnu image manipulation program
    glow                 # terminal markdown viewer
    hyperfine            # command-line benchmarking tool
    imagemagick          # image manipulation
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
    ncspot               # music streaming
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    unzip                # unzip files
    virt-manager         # virtual machine manager
    wally-cli            # ErgoDox EZ keyboard layout manager
    wayshot              # screenshot tool
    wgetpaste            # paste to pastebin
    wl-gammactl          # wayland gamma control
    wl-clipboard         # wayland clipboard manager
    xarchiver            # archive manager
    xclip                # command-line interface to X selections
    zathura              # document viewer
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
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
