{pkgs, ...}: {
  imports = [
    ../common
    ./desktop
    ./services
  ];

  nixpkgs.config.allowUnfree = true;

  home = {
    username = "mathoa";
    homeDirectory = "/home/mathoa";
  };

  home.packages = with pkgs; [];

  # Enable home-manager and git
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
