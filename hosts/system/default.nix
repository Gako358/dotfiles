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
    ./greetd.nix
    ./hyprland.nix
    ./nfc.nix
    ./shell.nix
    # ./snapper.nix
    ./qemu.nix
    ./wally.nix
  ];
}
