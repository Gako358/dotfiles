{
  lib,
  pkgs,
  config,
  modulesPath,
  ...
}: {
  hardware.opengl.enable = true;

  environment.systemPackages = with pkgs; [
    git
    home-manager
  ];

  users.users.zero = {
    isNormalUser = true;
    home = "/home/merrinx";
    shell = pkgs.zsh;
    extraGroups = ["wheel"];
  };
}
