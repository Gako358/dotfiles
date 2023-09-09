{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  myfonts = pkgs.callPackage ./system/fonts/default.nix {inherit pkgs;};
in {
  imports = [
    ./system
  ];
  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    extraHosts = ''
      127.0.0.1 tuathaan
      104.199.65.124 ap-gew4.spotify.com
    '';
  };
  nix = {
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
  };
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];
  # Add dconf settings
  programs.dconf.enable = true;
  services = {
    blueman.enable = true;
    dbus.enable = true;
    dbus.packages = [pkgs.gnome.gnome-keyring pkgs.gcr];
    gnome.gnome-keyring = {
      enable = true;
    };
    openssh = {
      enable = true;
      # Forbid root login through SSH.
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };

  fonts.packages = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "DejaVuSansMono"
        "FiraCode"
        "FiraMono"
        "Hack"
        "Iosevka"
        "JetBrainsMono"
        "LiberationMono"
        "Noto"
        "RobotoMono"
        "SourceCodePro"
        "UbuntuMono"
      ];
    })
    cascadia-code
    corefonts
    fira-code
    fira-code-symbols
    liberation_ttf
    material-design-icons
    noto-fonts
    noto-fonts-cjk
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-extra
    source-code-pro
    source-sans-pro
    unifont
    vistafonts
    vistafonts-chs
    vistafonts-cht
    font-awesome

    myfonts.flags-world-color
    myfonts.icomoon-feather
  ];

  hardware = {
    opengl.driSupport = true;
    keyboard.zsa.enable = true;
    bluetooth.enable = true;
    # Enable braodcom chip for bluetooth
    enableAllFirmware = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
      package = pkgs.pulseaudioFull;
    };
  };
  # Enable virtualisation and docker support
  virtualisation = {
    podman.enable = true;
    libvirtd.enable = true;
    docker = {
      enable = true;
      daemon.settings = {
        data-root = "/opt/docker";
      };
    };
  };
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "23.11";
}
