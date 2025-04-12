{ pkgs
, ...
}:
{
  imports = [
    ./cachix
    ./desktop.nix
    ./docker.nix
    ./fonts.nix
    ./qemu.nix
    ./shell.nix
    ./xdg.nix
  ];

  environment.systemPackages = with pkgs; [
    wget
    curl
    git
  ];
}
