{pkgs, ...}: let
  username = "merrinx";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  defaultPkgs = with pkgs; [
    acpi                 # battery info
    arandr               # screen layout manager
    asciinema            # record the terminal
    btop                 # alternative to htop & ytop
    cacert               # ca certificates
    cobang               # qr-code scanner
    cowsay               # cowsay fortune teller with random images
    dconf2nix            # dconf (gnome) files to nix converter
    docker-compose       # docker manager
    dive                 # explore docker layers
    discord              # messaging client
    drawio               # diagram design
    duf                  # disk usage/free utility
    exa                  # a better `ls`
    fd                   # "find" for files
    geany                # text editor
    gimp                 # gnu image manipulation program
    glow                 # terminal markdown viewer
    hyperfine            # command-line benchmarking tool
    killall              # kill processes by name
    lazygit              # terminal git ui
    libsecret            # secret management
    ncdu                 # disk space info (a better du)
    ncspot               # ncurses spotify client
    nitch                # minimal system information fetch
    nix-index            # locate packages containing certain nixpkgs
    nix-output-monitor   # nom: monitor nix commands
    nyancat              # the famous rainbow cat!
    ouch                 # painless compression and decompression for your terminal
    pavucontrol          # pulseaudio volume control
    paprefs              # pulseaudio preferences
    prettyping           # a nicer ping
    pulsemixer           # pulseaudio mixer
    rage                 # encryption tool for secrets management
    ripgrep              # fast grep
    scrot                # screenshot tool
    slack                # messaging client
    teams                # messaging client
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    unzip                # unzip files
    virt-manager         # virtual machine manager
    wally-cli            # ErgoDox EZ keyboard layout manager
    wgetpaste            # paste to pastebin
    xarchiver            # archive manager
    xclip                # command-line interface to X selections
    xdotool              # command-line X11 automation tool
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

  # Default services
  services.pasystray.enable = true;   # pulseaudio system tray

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
