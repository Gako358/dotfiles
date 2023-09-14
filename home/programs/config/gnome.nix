{pkgs, ...}: {
  home.packages = with pkgs; [
    gnome.gnome-tweaks
    gnomeExtensions.user-themes
    gnomeExtensions.tray-icons-reloaded
    gnomeExtensions.vitals
    gnomeExtensions.dash-to-panel
    gnomeExtensions.sound-output-device-chooser
    gnomeExtensions.space-bar
    flat-remix-gnome
  ];
#  dconf.settings = {
#    "org/gnome/tweaks".show-extensions-notice = false;
#    "org/gnome/shell" = {
#      disable-user-extensions = false;
#      enabled-extensions = [
#        "user-theme@gnome-shell-extensions.gcampax.github.com"
#        "trayIconsReloaded@selfmade.pl"
#        "Vitals@CoreCoding.com"
#        "dash-to-panel@jderose9.github.com"
#        "sound-output-device-chooser@kgshank.net"
#        "space-bar@luchrioh"
#      ];
#      favorite-apps = [
#        "firefox.desktop"
#        "thunderbird.desktop"
#        "alacritty.desktop"
#        "org.gnome.Nautilus.desktop"
#        "code.desktop"
#        "spotify.desktop"
#        "virt-manager.desktop"
#      ];
#    };
#    "org/gnome/shell/extensions/user-theme".name = "Flat-Remix-Blue-Light";
#    "org/gnome/desktop/interface" = {
#      color-scheme = "prefer-dark";
#      enable-hot-corners = false;
#    };
#    "org/gnome/desktop/wm/preferences" = {
#      workspace-names = ["Main"];
#    };
#  };
}
