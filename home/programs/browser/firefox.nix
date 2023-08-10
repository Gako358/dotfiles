{pkgs, ...}: let
  mailClient = pkgs.thunderbird;
in {
  programs.firefox = {
    enable = true;
    package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
      extraPolicies = {
        ExtensionSettings = {};
      };
    };
  };
  home.packages = [mailClient];
}
