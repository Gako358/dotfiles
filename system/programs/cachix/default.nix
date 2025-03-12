{ pkgs
, ...
}:
{
  imports = [
    ./merrinx.nix
    ./nix-community.nix
  ];
  nix.settings.substituters = [ "https://cache.nixos.org/" ];

  environment.systemPackages = [ pkgs.cachix ];
}
