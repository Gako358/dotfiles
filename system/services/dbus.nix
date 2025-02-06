{pkgs, ...}: {
  services = {
    dbus = {
      enable = true;
      implementation = "broker";
      packages = [pkgs.gnome-keyring pkgs.gcr];
    };
  };
}
