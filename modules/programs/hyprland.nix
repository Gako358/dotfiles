{ lib
, pkgs
, osConfig
, ...
}:
{

  config = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
    home = {
      packages = [
        pkgs.arandr # screen layout manager
        pkgs.bottom # alternative to htop & ytop
        pkgs.ffmpegthumbnailer # thumbnailer for video files
        pkgs.glib # Core application building blocks library (used by GTK/GNOME apps)
        pkgs.gnome-calendar # Calendar application from the GNOME desktop
        pkgs.gnome-boxes # Simple virtual machine manager from GNOME
        pkgs.gnome-weather # Weather application from the GNOME desktop
        pkgs.gnome-system-monitor # System resource monitor from GNOME
        pkgs.headsetcontrol # control logitech headsets
        pkgs.imagemagick # image manipulation
        pkgs.paprefs # pulseaudio preferences
        pkgs.pavucontrol # pulseaudio volume control
        pkgs.poppler # pdf tools
        pkgs.pulsemixer # pulseaudio volume control
        pkgs.ranger # file manager
        pkgs.scrot # screenshot tool
        pkgs.slurp # select a region in a wayland compositor
        pkgs.wayshot # screenshot tool
        pkgs.wgetpaste # paste to pastebin
        pkgs.wl-clipboard # wayland clipboard manager
        pkgs.wl-gammactl # wayland gamma control
      ];
      sessionVariables = {
        SSH_AUTH_SOCK = "/run/user/1000/keyring/ssh";
        XDG_CURRENT_DESKTOP = "Hyprland";
        XDG_SESSION_DESKTOP = "Hyprland";
        XDG_SESSION_TYPE = "wayland";
        SDL_VIDEODRIVER = "wayland";
        QT_AUTO_SCREEN_SCALE_FACTOR = 1;
        QT_QPA_PLATFORM = "wayland;xcb";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
        QT_QPA_PLATFORMTHEME = "qt5ct";
      };
    };
  };
}
