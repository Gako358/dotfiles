{ config
, inputs
, lib
, pkgs
, ...
}: {
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

  # Set the default editor
  environment.variables.EDITOR = "nvim";
  security = {
    rtkit.enable = true;
    pam.services.swaylock = { };
  };

  # Secret managed by sops-nix
  sops = {
    defaultSopsFile = ../secrets/default.yaml;
    validateSopsFiles = false;
    secrets = {
      email-passwd = { };
      email-user = { };
    };

    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };
  };
  # Set default shell to fish global
  users.defaultUserShell = pkgs.fish;
}
