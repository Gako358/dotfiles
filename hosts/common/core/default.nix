{pkgs, ...}: {
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
    openssh
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

    nodejs-16_x # Github Copilot requires nodejs 16
    alejandra # Nix formatting tool
  ];

  # Enable the NixOS module system
  programs.dconf.enable = true;

  imports = [
    ./fonts.nix
  ];
}
