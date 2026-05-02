_: {
  flake.nixosModules.programs-quickshell =
    {
      lib,
      config,
      ...
    }:
    {
      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        security.pam.services.quickshell = { };
      };
    };

  flake.homeModules.programs-quickshell =
    {
      osConfig,
      config,
      pkgs,
      lib,
      ...
    }:
    let
      cfg = config.program.quickshell;
      inherit (config.colorScheme) palette;

      c = name: "#${palette.${name}}";
      ca = name: alpha: "#${alpha}${palette.${name}}";

      shellQml = import ./_shell.nix;
      barQml = import ./_bar.nix {
        inherit c ca lib;
        battery = cfg.battery.enable;
      };
      dashboardQml = import ./_dashboard.nix { inherit c ca; };
      notificationsQml = import ./_notifications.nix { inherit c ca; };
      launcherQml = import ./_launcher.nix { inherit c ca; };
      sessionQml = import ./_session.nix { inherit c ca; };
      lockQml = import ./_lock.nix { inherit c ca; };
      sysmonQml = import ./_system-monitor.nix { inherit c ca; };
      volumePanelQml = import ./_volume-panel.nix { inherit c ca; };
      networkPanelQml = import ./_network-panel.nix { inherit c ca; };

      bivrostConfig = pkgs.runCommand "quickshell-bivrost" { } ''
        mkdir -p $out
        cp ${pkgs.writeText "shell.qml" shellQml}                 $out/shell.qml
        cp ${pkgs.writeText "Bar.qml" barQml}                     $out/Bar.qml
        cp ${pkgs.writeText "Dashboard.qml" dashboardQml}         $out/Dashboard.qml
        cp ${pkgs.writeText "Notifications.qml" notificationsQml} $out/Notifications.qml
        cp ${pkgs.writeText "Launcher.qml" launcherQml}           $out/Launcher.qml
        cp ${pkgs.writeText "Session.qml" sessionQml}             $out/Session.qml
        cp ${pkgs.writeText "Lock.qml" lockQml}                   $out/Lock.qml
        cp ${pkgs.writeText "SystemMonitor.qml" sysmonQml}        $out/SystemMonitor.qml
        cp ${pkgs.writeText "VolumePanel.qml" volumePanelQml}     $out/VolumePanel.qml
        cp ${pkgs.writeText "NetworkPanel.qml" networkPanelQml}   $out/NetworkPanel.qml
      '';
    in
    {
      options.program.quickshell = {
        battery = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable the battery widget in the quickshell bar (laptops only).";
          };
        };
      };

      config.programs.quickshell = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
        enable = true;
        package = pkgs.quickshell;

        configs.bivrost = bivrostConfig;
        activeConfig = "bivrost";

        systemd = {
          enable = true;
          target = "graphical-session.target";
        };
      };
    };
}
