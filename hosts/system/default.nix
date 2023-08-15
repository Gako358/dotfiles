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
    ./daemons
    ./desktop
    ./files.nix
    ./fonts.nix
    ./lock.nix
    ./nfc.nix
    ./zsh.nix
  ];
}
