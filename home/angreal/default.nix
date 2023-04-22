{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../common/services/shell.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home = {
    username = "nixos";
    homeDirectory = "/home/nixos";
  };

  home.packages = with pkgs; [];

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "Gako358";
    userEmail = "gako358@outlook.com";
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.05";
}
