_: {
  flake.nixosModules.services-greetd =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      options.environment.desktop.greeter.monitors = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "DP-2" ];
        description = ''
          Ordered list of monitor identifiers. The greeter UI is drawn on the
          first connected monitor whose name / model / serial number contains
          one of these strings; other monitors get a plain dark surface.
          "desc:" prefixes are stripped before matching. When the list is empty
          (the default), the UI is shown on every connected monitor.
        '';
      };

      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        security = {
          pam = {
            loginLimits = [
              {
                domain = "*";
                type = "soft";
                item = "nofile";
                value = "65536";
              }
              {
                domain = "*";
                type = "hard";
                item = "nofile";
                value = "1048576";
              }
            ];
            services.greetd.enableGnomeKeyring = true;
            services.swaylock = { };
          };
          polkit.enable = true;
          rtkit.enable = true;
        };
        services = {
          gvfs.enable = true;
          devmon.enable = true;
          udisks2.enable = true;
          upower.enable = true;
          accounts-daemon.enable = true;

          greetd =
            let
              palette = import ../themes/_palette.nix;
              c = name: "#${palette.${name}}";
              ca = name: alpha: "#${alpha}${palette.${name}}";

              uwsmExe = lib.getExe config.programs.uwsm.package;

              sessionArgs = [
                "start"
                "-e"
                "-D"
                "Hyprland"
                "hyprland-uwsm.desktop"
              ];

              greeterQml = import ../programs/quickshell/_greeter.nix {
                inherit lib c ca;
                user = "merrinx";
                sessionCommand = [ uwsmExe ] ++ sessionArgs;
                greeterMonitors = config.environment.desktop.greeter.monitors;
              };
              greeterConfig = pkgs.runCommand "quickshell-greeter" { } ''
                mkdir -p $out
                cp ${pkgs.writeText "shell.qml" greeterQml} $out/shell.qml
              '';
            in
            {
              enable = true;
              restart = true;
              settings = {
                terminal.vt = 1;
                default_session = {
                  command = "${pkgs.cage}/bin/cage -s -- ${pkgs.coreutils}/bin/env QT_WAYLAND_DISABLE_WINDOWDECORATION=1 ${pkgs.quickshell}/bin/quickshell -p ${greeterConfig}/shell.qml";
                  user = "greeter";
                };
              };
            };
        };
      };
    };
}
