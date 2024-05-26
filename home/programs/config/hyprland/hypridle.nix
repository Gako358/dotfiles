{
  pkgs,
  lib,
  config,
  specialArgs,
  ...
}: let
  suspendScript = pkgs.writeShellScript "suspend-script" ''
    ${pkgs.pipewire}/bin/pw-cli i all 2>&1 | ${pkgs.ripgrep}/bin/rg running -q
    # only suspend if audio isn't running
    if [ $? == 1 ]; then
      ${pkgs.systemd}/bin/systemctl suspend
    fi
  '';

  brillo = lib.getExe pkgs.brillo;
in {
  # screen idle
  services.hypridle = {
    enable = true;

    settings = {
      general = {
        lock_cmd = lib.getExe config.programs.hyprlock.package;
        before_sleep_cmd = "${pkgs.systemd}/bin/loginctl lock-session";
      };

      listener = [
        {
          timeout =
            if specialArgs.hidpi
            then 3600
            else 1140; # Suspend on 1 hour for hidpi, 19 minutes for normal
          on-timeout = suspendScript.outPath;
        }
        {
          timeout =
            if specialArgs.hidpi
            then 3000
            else 600; # 50 minutes for hidpi, 10 minutes for normal
          # save the current brightness and dim the screen over a period of
          # 1 second
          on-timeout = "${brillo} -O; ${brillo} -u 1000000 -S 10";
          # brighten the screen over a period of 500ms to the saved value
          on-resume = "${brillo} -I -u 500000";
        }
      ];
    };
  };
}
