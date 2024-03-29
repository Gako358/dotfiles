{pkgs, ...}: let
  username = "merrinx";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    any-nix-shell # fish support for nix shell
    acpi # battery info
    arandr # screen layout manager
    brightnessctl # control screen brightness
    bottom # alternative to htop & ytop
    cacert # ca certificates
    dbeaver # database manager
    dconf2nix # dconf (gnome) files to nix converter
    dive # explore docker layers
    duf # disk usage/free utility
    eza # a better `ls`
    fd # "find" for files
    gimp # gnu image manipulation program
    github-copilot-cli # github copilot cli
    jump # fast directory navigation
    killall # kill processes by name
    lazygit # terminal git ui
    libsecret # secret management
    ncdu # disk space info (a better du)
    nitch # minimal system information fetch
    nix-index # locate packages containing certain nixpkgs
    nix-output-monitor # nom: monitor nix commands
    ouch # painless compression and decompression for your terminal
    paprefs # pulseaudio preferences
    prettyping # a nicer ping
    rage # encryption tool for secrets management
    ripgrep # fast grep
    rnote # terminal note taking
    scrot # screenshot tool
    slurp # select a region in a wayland compositor
    spotify # music streaming
    tldr # summary of a man page
    tree # display files in a tree view
    unzip # unzip files
    wgetpaste # paste to pastebin
    wl-gammactl # wayland gamma control
    wl-clipboard # wayland clipboard manager
    xarchiver # archive manager
    zip # zip files
  ];
in {
  programs = {
    home-manager.enable = true;
    gh.enable = true;
  };
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

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "23.11";
    packages = defaultPkgs;
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
