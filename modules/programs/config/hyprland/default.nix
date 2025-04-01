{ pkgs
, inputs
, ...
}: {
  imports = [
    ./binds.nix
    ./hypridle.nix
    ./hyprland.nix
    ./hyprlock.nix
    ./hyprpaper.nix
    ./monitors.nix
    ./rules.nix
    ./systemd-fixes.nix
    ./tty.nix
    ./variables.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
      extraCommands = [
        "systemctl --user stop graphical-session.target"
        "systemctl --user start hyprland-session.target"
      ];
    };

    settings = {
      cursor.inactive_timeout = 5;
    };
  };

  home.packages = [
    inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
    pkgs.discord # video streaming app
    pkgs.ffmpegthumbnailer # thumbnailer for video files
    pkgs.gimp # gnu image manipulation program
    pkgs.headsetcontrol # control logitech headsets
    pkgs.imagemagick # image manipulation
    pkgs.paprefs # pulseaudio preferences
    pkgs.pavucontrol # pulseaudio volume control
    pkgs.poppler # pdf tools
    pkgs.pulsemixer # pulseaudio volume control
    pkgs.scrot # screenshot tool
    pkgs.slack # messaging app
    pkgs.slurp # select a region in a wayland compositor
    pkgs.spotify # music streaming
    pkgs.wayshot # screenshot tool
    pkgs.wgetpaste # paste to pastebin
    pkgs.wl-clipboard # wayland clipboard manager
    pkgs.wl-gammactl # wayland gamma control
  ];

  # Fake a tray, so apps can start
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = [ "graphical-session-pre.target" ];
    };
  };
}
