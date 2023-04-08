{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  citrixFocusFixScript = pkgs.writeShellScript "citrix-focus-fix" ''
    export PATH=${lib.makeBinPath [
      pkgs.bash
      pkgs.coreutils
      pkgs.xorg.xprop
      pkgs.xorg.xdotool
    ]}

    # Replace this with the correct value for your Citrix application
    citrix_window_class="Citrix Receiver"

    # Get the active workspace
    active_workspace=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}')

    # Find the Citrix window and its workspace
    citrix_window_id=""
    citrix_workspace=""

    for window_id in $(xprop -root _NET_CLIENT_LIST | awk -F'# ' '{print $2}' | tr -d ',' | tr -d ' '); do
      window_class=$(xprop -id $window_id WM_CLASS)
      if echo "$window_class" | grep -q "$citrix_window_class"; then
          citrix_window_id="$window_id"
          citrix_workspace=$(xprop -id $window_id _NET_WM_DESKTOP | awk '{print $3}')
          break
      fi
    done

    if [ -n "$citrix_window_id" ] && [ "$active_workspace" = "$citrix_workspace" ]; then
      xdotool windowfocus "$citrix_window_id"
    fi
  '';

  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm") {
    systemd.user.services.citrix-focus-fix = {
      description = "Fix Citrix window focus";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${citrixFocusFixScript}";
      };
      wantedBy = ["multi-user.target"];
    };

    systemd.user.timers.citrix-focus-fix = {
      description = "Run Citrix window focus fix every few seconds";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnUnitActiveSec = "5s";
        Unit = "citrix-focus-fix.service";
      };
    };
  };
}
