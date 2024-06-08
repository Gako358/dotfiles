{
  lib,
  pkgs,
  config,
  specialArgs,
  ...
}: {
  # greetd display manager
  services.greetd = let
    session = {
      command =
        if specialArgs.hidpi
        then "${lib.getExe config.programs.hyprland.package}"
        else "${pkgs.greetd.tuigreet}/bin/tuigreet --time --time-format '%I:%M %p | %a • %h | %F' --cmd Hyprland";
      user = "merrinx";
    };
  in {
    enable = true;
    restart = true;
    settings = {
      terminal.vt = 1;
      default_session = session;
    };
  };

  # unlock GPG keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
