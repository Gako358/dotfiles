{ pkgs, ... }: {
  programs.dconf.enable = true;
  services = {
    dbus = {
      enable = true;
      implementation = "broker";
      packages = [ pkgs.gnome-keyring pkgs.gcr ];
    };
  };
}
