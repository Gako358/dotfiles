{
  imports = [
    ./graphics.nix
    ./network.nix
  ];

  hardware = {
    keyboard.zsa.enable = true;
    enableAllFirmware = true;
  };

  # Enable proprietary software
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "24.11";
}
