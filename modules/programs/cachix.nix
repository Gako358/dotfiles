_: {
  flake.nixosModules = {
    programs-cachix =
      {
        pkgs,
        lib,
        ...
      }:
      {
        nix.settings.substituters = lib.mkAfter [ "https://cache.nixos.org/" ];
        environment.systemPackages = [ pkgs.cachix ];
      };

    programs-cachix-hl = _: {
      nix.settings = {
        substituters = [ "https://hyprland.cachix.org" ];
        trusted-substituters = [ "https://hyprland.cachix.org" ];
        trusted-public-keys = [
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        ];
      };
    };

    programs-cachix-iog = _: {
      nix.settings = {
        substituters = [ "https://cache.iog.io" ];
        trusted-public-keys = [
          "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        ];
      };
    };

    programs-cachix-merrinx = _: {
      nix.settings = {
        substituters = [ "https://merrinx.cachix.org" ];
        trusted-public-keys = [
          "merrinx.cachix.org-1:xxPuUGRPdYkH1eAUy1hLXt9w3GCfKTFVrkiWdlPEN9E="
        ];
      };
    };

    programs-cachix-nix-community = _: {
      nix.settings = {
        substituters = [ "https://nix-community.cachix.org" ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
      };
    };
  };
}
