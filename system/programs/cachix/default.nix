{ pkgs
, lib
, ...
}:
{
  imports = [
    ./merrinx.nix
    ./nix-community.nix
  ];
  nix.settings.substituters = lib.mkAfter [ "https://cache.nixos.org/" ];

  environment.systemPackages = [ pkgs.cachix ];
}
