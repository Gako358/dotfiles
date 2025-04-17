{ config, pkgs, ... }:
let

  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [ ../../default.nix ];

  programs.hyprlock.settings.input-field = [{ monitor = "DP-2"; }];
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=DP-2,2560x1440,0x0,1
    monitor=DP-3,2560x1440,2560x0,1
    monitor=,highrr,auto,1

    workspace = 1, monitor:DP-3
    workspace = 2, monitor:DP-2
    workspace = 3, monitor:DP-2
    workspace = 4, monitor:DP-2
    workspace = 5, monitor:DP-2
    workspace = 6, monitor:DP-3
    workspace = 7, monitor:DP-3
    workspace = 8, monitor:DP-3
    workspace = 9, monitor:DP-3
  '';

  service.mail = {
    enable = true;
    password = "${cat} ${config.sops.secrets."email_home-passwd".path}";
  };
}
