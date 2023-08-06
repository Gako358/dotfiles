{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.services.garbageCollector;
in {
  options = {
    services.garbageCollector = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the garbage collector";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm")) {
    systemd.services.garbageCollector = {
      description = "Check disk usage and perform garbage collection if necessary";
      serviceConfig = {
        ExecStart = let
          script = pkgs.writeShellScript "check-garbage" ''
            if [ $(df /boot/efi | awk 'NR==2 {print $5}' | sed 's/%//') -ge 50 ]; then
              nix-collect-garbage -d
              export DISPLAY=:0
              export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
              dunstify -u critical "Garbage collection performed"
            fi
          '';
        in "${script}/bin/check-garbage";
        Type = "oneshot";
        RemainAfterExit = "yes";
        User = "root";
      };
    };

    systemd.timers.garbageCollector = {
      wantedBy = ["timers.target"];
      timerConfig = {
        OnCalendar = "hourly";
        Persistent = true;
      };
    };
  };
}
