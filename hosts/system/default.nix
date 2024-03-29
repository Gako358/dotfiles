{
  pkgs,
  lib,
  ...
}:
with lib; {
  # Core pakages for system
  environment.systemPackages = with pkgs; [
    wget
    curl
    git

    nodejs-18_x # Github Copilot requires nodejs 16
    alejandra # Nix formatting tool
  ];

  imports = [
    ./docker.nix
    ./fonts.nix
    ./gnome.nix
    ./hyprland.nix
    ./nfc.nix
    ./shell.nix
    ./qemu.nix
    ./wally.nix
  ];
}
