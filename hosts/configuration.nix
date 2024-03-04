{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./system
  ];
  # NixOS Network Configuration
  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    extraHosts = ''
      127.0.0.1 tuathaan
      104.199.65.124 ap-gew4.spotify.com
    '';
  };
  # NixOS enable Flakes
  nix = {
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
    # Weekly garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    # Trusted users
    settings.trusted-users = ["root" "merrinx" "@wheel"];

    # Enable optimisation
    optimise = {
      automatic = true;
      dates = ["weekly"];
    };
  };
  # Timezone and locale
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];
  console.useXkbConfig = true;
  programs = {
    dconf.enable = true;
    virt-manager.enable = true;
  };
  services = {
    blueman.enable = true;
    dbus.packages = [pkgs.gnome.gnome-keyring pkgs.gcr];
    gnome.gnome-keyring = {
      enable = true;
    };
    # Enable SSH
    openssh = {
      enable = true;
      # Forbid root login through SSH.
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
    # Enable pipewire
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };
    udev.packages = [
      (pkgs.writeTextFile {
        name = "wally_udev";
        text = ''
          # Rules for Oryx web flashing and live training
          KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
          KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"

          # Legacy rules for live training over webusb (Not needed for firmware v21+)
            # Rule for all ZSA keyboards
            SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
            # Rule for the Moonlander
            SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
            # Rule for the Ergodox EZ
            SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="1307", GROUP="plugdev"
            # Rule for the Planck EZ
            SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="6060", GROUP="plugdev"

          # Wally Flashing rules for the Ergodox EZ
          ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
          ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
          KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

          # Keymapp / Wally Flashing rules for the Moonlander and Planck EZ
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666", SYMLINK+="stm32_dfu"
          # Keymapp Flashing rules for the Voyager
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="3297", MODE:="0666", SYMLINK+="ignition_dfu"
        '';
        destination = "/etc/udev/rules.d/50-zsa.rules";
      })
    ];
  };
  sound.enable = true;
  security = {
    rtkit.enable = true;
    pam.services.swaylock = {};
  };

  # Enable hardware support
  hardware = {
    opengl.enable = true;
    opengl.driSupport = true;
    keyboard.zsa.enable = true;
    bluetooth.enable = true;
    # Enable braodcom chip for bluetooth
    enableAllFirmware = true;
    pulseaudio.enable = false;
  };
  # Enable virtualisation and docker support
  virtualisation = {
    podman.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        swtpm.enable = true;
        ovmf.enable = true;
        ovmf.packages = [pkgs.OVMFFull.fd];
      };
    };
    spiceUSBRedirection.enable = true;
    docker = {
      enable = true;
      daemon.settings = {
        data-root = "/opt/docker";
      };
    };
  };
  # Packages for virtualisation
  environment.systemPackages = with pkgs; [
    spice
    spice-gtk
    spice-protocol
    virt-viewer
    virtio-win
    win-spice
  ];
  # Set default shell to fish global
  users.defaultUserShell = pkgs.fish;
  # Enable proprietary software
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "23.11";
}
