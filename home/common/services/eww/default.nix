{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  dependencies = with pkgs; [
    config.programs.eww.package
    bash
    bc
    blueberry
    bluez
    coreutils
    dbus
    dunst
    findutils
    gawk
    gnused
    iwgtk
    jq
    light
    networkmanager
    networkmanagerapplet
    pavucontrol
    playerctl
    procps
    pulseaudio
    ripgrep
    socat
    udev
    upower
    util-linux
    wget
    wireplumber
    wlogout
    wofi
  ];
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "bspwm") {
    programs.eww = {
      enable = true;
      # package = inputs.eww.packages.${pkgs.system}.eww-wayland;
      # remove nix files
      configDir = lib.cleanSourceWith {
        filter = name: _type: let
          baseName = baseNameOf (toString name);
        in
          !(lib.hasSuffix ".nix" baseName);
        src = lib.cleanSource ./.;
      };
    };

    systemd.user.services.eww = {
      Unit = {
        Description = "Eww Daemon";
        # not yet implemented
        # PartOf = ["tray.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Environment = "PATH=/run/wrappers/bin:${lib.makeBinPath dependencies}";
        ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
        Restart = "on-failure";
      };
      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
