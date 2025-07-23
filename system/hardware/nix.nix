{ inputs
, config
, lib
, ...
}:
{
  users.groups.nix-access-tokens = { };
  sops = lib.mkIf config.service.sops.enable {
    secrets."github_token" = {
      restartUnits = [ "nix-daemon.service" ];
    };

    templates."nix-access-tokens" = {
      content = ''
        access-tokens = github.com=${config.sops.placeholder."github_token"}
      '';
      group = config.users.groups.nix-access-tokens.name;
      mode = "0440";
    };
  };

  nix = {
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      experimental-features = "nix-command flakes impure-derivations ca-derivations";
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "merrinx"
        "@wheel"
      ];
      download-buffer-size = 524288000;
    };
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
    extraOptions = lib.mkIf config.service.sops.enable ''
      !include ${config.sops.templates."nix-access-tokens".path}
    '';
  };
}
