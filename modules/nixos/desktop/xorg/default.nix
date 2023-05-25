{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop.xsession;
in {
  options.desktop.xsession.enable = lib.mkEnableOption "xsession";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      scrot
      arandr
      ranger
      ncspot
      zathura
    ];
    # Add the following to your /etc/hosts
    networking.extraHosts = ''
      104.199.65.124 ap-gew4.spotify.com
    '';

    # Add dconf settings
    programs.dconf.enable = true;

    # Grant slock root access
    # security.wrappers.slock = {
    #   setuid = true;
    #   setgid = true;
    #   source = "${pkgs.slock}/bin/slock";
    #   owner = "root";
    #   group = "root";
    # };
  };
}
