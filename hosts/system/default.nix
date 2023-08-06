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
    ./work
    ./files.nix
    ./fonts.nix
    ./lock.nix
    ./zsh.nix
  ];
}
