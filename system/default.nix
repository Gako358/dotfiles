{ config
, inputs
, pkgs
, lib
, ...
}: {
  imports = [
    ./environment
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
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    settings.trusted-users = [ "root" "merrinx" "@wheel" ];
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
  };

  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];

  console.useXkbConfig = true;
  programs.fuse.userAllowOther = true;

  users = {
    defaultUserShell = pkgs.fish;
    mutableUsers = false;
    users.root.initialHashedPassword = "$6$pbE4rcxk1KsvypJn$ZJlFtw85hgSWzdJnAuZr935zmm6Qc974ehL13/8WGPKnxX4epK5FiP2BBM/q89Gii9Xk0hxVvVKOkdkZuR2xE0";
  };
}
