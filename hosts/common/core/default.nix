{
  pkgs,
  lib,
  ...
}:
with lib; {
  # Core pakages for system
  environment.systemPackages = with pkgs; [
    # Nix Core
    nix-index
    wgetpaste
    neofetch
    ripgrep
    cmatrix
    xdotool
    lazygit
    pstree
    cacert
    unzip
    xclip
    acpi
    btop
    fd

    # Archive tools
    xarchiver
    wget
    curl
    zip
    git

    nodejs-18_x # Github Copilot requires nodejs 16
    alejandra # Nix formatting tool
  ];

  imports = [
    ./fonts.nix
  ];
}
