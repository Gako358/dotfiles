{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs = {
    chromium.enable = true;
    zen.enable = false;
  };
  home.file = lib.mkIf (config.home.username == "farstrider") {
    ".config/autostart/chromium-browser.desktop".source =
      "${pkgs.chromium}/share/applications/chromium-browser.desktop";
  };
  services.dconf.defaultInputSource = "no";
  dconf.settings."org/gnome/shell/extensions/caffeine" = {
    restore-state = lib.mkForce false;
    user-enabled = lib.mkForce false;
  };
}
