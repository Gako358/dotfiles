{
  pkgs,
  config,
  specialArgs,
  ...
}:
if !specialArgs.hidpi
then let
  swaylock = "${config.programs.swaylock.package}/bin/swaylock";
  pgrep = "${pkgs.procps}/bin/pgrep";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";

  isLocked = "${pgrep} -x ${swaylock}";
  lockTime = 20 * 60;
  screenDelay = 20; # 20 seconds
  micMute = 10; # 10 seconds

  # Makes two timeouts: one for when the screen is not locked (lockTime+timeout) and one for when it is.
  afterLockTimeout = {
    timeout,
    command,
    resumeCommand ? null,
  }: [
    {
      timeout = lockTime + timeout;
      inherit command resumeCommand;
    }
    {
      command = "${isLocked} && ${command}";
      inherit resumeCommand timeout;
    }
  ];
in {
  services.swayidle = {
    enable = true;
    systemdTarget = "graphical-session.target";
    timeouts =
      # Lock screen
      [
        {
          timeout = lockTime;
          command = "${swaylock} -f -c 000000";
        }
      ]
      ++
      # Mute mic 10 seconds after locking
      (afterLockTimeout {
        timeout = micMute;
        command = "${pactl} set-source-mute @DEFAULT_SOURCE@ yes";
        resumeCommand = "${pactl} set-source-mute @DEFAULT_SOURCE@ no";
      })
      ++
      # Turn off screen 60 seconds after locking timer
      # Suspends system 30 minutes after the screen is turned off
      # 31 min if hidpi
      # 11 min if not hidpi
      (afterLockTimeout {
        timeout = screenDelay;
        command = "${hyprctl} dispatch dpms off";
        resumeCommand = "${hyprctl} dispatch dpms on";
      });
  };
}
else {}
