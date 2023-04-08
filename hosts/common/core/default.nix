{pkgs, ...}: {
  # Core pakages for system
  environment.systemPackages = with pkgs; [
    # Nix Core
    nix-index
    wgetpaste
    neofetch
    ripgrep
    xdotool
    lazygit
    pstree
    unzip
    xclip
    fd

    # GUI Core
    thunderbird
    firefox
    cmatrix
    discord
    geany
    gimp
    acpi
    btop

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
