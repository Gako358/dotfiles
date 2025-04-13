{ config
, inputs
, lib
, pkgs
, ...
}: {
  imports = [
    ./hardware
    ./programs
    ./services
  ];
  # NixOS enable Flakes
  nix = {
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
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
    settings.trusted-users = [ "root" "merrinx" "@wheel" ];

    # Enable optimisation
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
  };
  # Timezone and locale
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];
  console.useXkbConfig = true;

  # Impermanence
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/ssh"
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
      "/var/log"

      # Systemd requires /usr dir to be populated
      # See: https://github.com/nix-community/impermanence/issues/253
      "/usr/systemd-placeholder"
    ];
    users.merrinx = {
      directories = [
        { directory = ".gnupg"; mode = "0700"; }
        { directory = ".ssh"; mode = "0700"; }
        { directory = ".local/share/keyrings"; mode = "0700"; }
      ];
    };
  };

  programs.fuse.userAllowOther = true;

  users = {
    defaultUserShell = pkgs.fish;
    mutableUsers = false;
    users.root.initialHashedPassword = "$6$pbE4rcxk1KsvypJn$ZJlFtw85hgSWzdJnAuZr935zmm6Qc974ehL13/8WGPKnxX4epK5FiP2BBM/q89Gii9Xk0hxVvVKOkdkZuR2xE0";
  };
}
