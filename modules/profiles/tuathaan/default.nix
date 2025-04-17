{ config, pkgs, ... }:
let

  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [ ../../default.nix ];

  programs.hyprlock.settings.input-field = [{ monitor = "DP-8"; }];
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=eDP-1,1920x1200,2560x1440,1
    monitor=DP-8,2560x1440,0x0,1
    monitor=DP-6,2560x1440,2560x0,1
    monitor=,highrr,auto,1

    workspace = 1, monitor:DP-6
    workspace = 2, monitor:DP-8
    workspace = 3, monitor:DP-8
    workspace = 4, monitor:DP-8
    workspace = 5, monitor:DP-8
    workspace = 6, monitor:DP-6
    workspace = 7, monitor:DP-6
    workspace = 8, monitor:DP-6
    workspace = 9, monitor:DP-6
  '';

  service.mail = {
    enable = true;
    password = "${cat} ${config.sops.secrets."email_work-passwd".path}";
  };
}
