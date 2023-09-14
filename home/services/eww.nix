{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  ewwKill = pkgs.writeShellScriptBin "eww-kill" ''
    if [ -x "$(command -v eww)" ]; then
      eww kill
    fi
  '';

  themeRon = pkgs.writeText "theme.ron" ''
    border_width: 1,
    margin: 0,
    default_border_color: "#37474F",
    floating_border_color: "#225588",
    focused_border_color: "#885522",
  '';

  workspaces = pkgs.writeText "workspaces" ''
    workspaces=(
      "1920x1080+0+0"
      "2560x1440+1920+0"
      "2560x1440+4480+0"
    )
  '';

  sizeLiquid = pkgs.writeShellScript "size-liquid" ''
    for workspace in "${workspaces}"; do
      echo "$workspace"
    done
  '';

  sizesOfLiquid = pkgs.writeShellScript "sizes" ''
    sizes=( $(${sizeLiquid}) )

  '';

  leftwmScript = pkgs.writeShellScript "leftwm-config" ''
    # If LD_LIBRARY_PATH is not exported, do so:
    if [ -z "$LD_LIBRARY_PATH" ]; then
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"
    fi

    # Down the last running theme
    if [ -f "/tmp/leftwm-theme-down" ]; then
      /tmp/leftwm-theme-down
      rm /tmp/leftwm-theme-down
    fi
    ln -s "${ewwKill}/bin/eww-kill" /tmp/leftwm-theme-down

    # Start eww daemon
    eww daemon &

    # Set the theme.ron config
    leftwm-command "LoadTheme ${themeRon}/bin/theme.ron"

    # Open eww 'bar' windows
    # This is a bit of an ugly hack, a more elegant way will hopefully be possible with a future `eww` version
    sleep 1
    index=0
    #for size in "${sizesOfLiquid}"; do
    #  eww open "bar$index"
    #  let index=index+1
    #done

    systemctl --user restart stalonetray
  '';
in {
  options = {
    services.leftwm = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable LeftWM and Eww setup";
      };
    };
  };
  config.systemd.user.services.leftwm = {
    Install.WantedBy = ["graphical-session.target"];
    Service.ExecStart = "${leftwmScript}";
  };
}
