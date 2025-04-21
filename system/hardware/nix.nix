{ inputs
, config
, lib
, ...
}: {
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
}
