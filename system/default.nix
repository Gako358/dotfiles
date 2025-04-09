{ config
, inputs
, lib
, pkgs
, ...
}:
let
  cat = "${pkgs.coreutils}/bin/cat";

in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
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
  security = {
    rtkit.enable = true;
    pam.services.swaylock = { };
  };

  users = {
    # Set default shell to fish global
    defaultUserShell = pkgs.fish;
    # Only immutable users
    mutableUsers = false;
    users.root.initialHashedPassword = "${cat} ${config.sops.secrets.user_password.path}";
  };
}
