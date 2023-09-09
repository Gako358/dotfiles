{
  config,
  pkgs,
  specialArgs,
  ...
}: let
  openCalendar = "${pkgs.xfce.orage}/bin/orage";

  hdmiBar = pkgs.callPackage ./bar.nix {};

  laptopBar = pkgs.callPackage ./bar.nix {
    font0 = 15;
    font1 = 17;
    font2 = 28;
    font3 = 25;
    font4 = 9;
    font5 = 15;
  };

  mainBar = hdmiBar;

  openGithub = "${pkgs.firefox-beta-bin}/bin/firefox -new-tab https\\://github.com/notifications";

  mypolybar = pkgs.polybar.override {
    alsaSupport = true;
    githubSupport = true;
    mpdSupport = true;
    pulseSupport = true;
  };

  # theme adapted from: https://github.com/adi1090x/polybar-themes#-polybar-5
  bars = builtins.readFile ./bars.ini;
  colors = builtins.readFile ./colors.ini;
  mods1 = builtins.readFile ./modules.ini;
  mods2 = builtins.readFile ./user_modules.ini;

  bluetoothScript = pkgs.callPackage ./scripts/bluetooth.nix {};
  klsScript = pkgs.callPackage ../../scripts/keyboard-layout-switch.nix {inherit pkgs;};
  monitorScript = pkgs.callPackage ./scripts/monitor.nix {};
  mprisScript = pkgs.callPackage ./scripts/mpris.nix {};
  networkScript = pkgs.callPackage ./scripts/network.nix {};

  bctl = ''
    [module/bctl]
    type = custom/script
    exec = ${bluetoothScript}/bin/bluetooth-ctl
    tail = true
    click-left = ${bluetoothScript}/bin/bluetooth-ctl --toggle &
  '';

  cal = ''
    [module/clickable-date]
    inherit = module/date
    label = %{A1:${openCalendar}:}%time%%{A}
  '';

  keyboard = ''
    [module/clickable-keyboard]
    inherit = module/keyboard
    label-layout = %{A1:${klsScript}/bin/kls:}  %layout% %icon% %{A}
  '';

  mpris = ''
    [module/mpris]
    type = custom/script

    exec = ${mprisScript}/bin/mpris
    tail = true

    label-maxlen = 60

    interval = 2
    format =   <label>
    format-padding = 2
  '';

  xmonad = ''
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log

    tail = true
  '';

  customMods = mainBar + bctl + cal + keyboard + mpris + xmonad;
in {
  home.packages = with pkgs; [
    font-awesome # awesome fonts
    material-design-icons # fonts with glyphs
    xfce.orage # lightweight calendar
  ];

  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./config.ini;
    extraConfig = bars + colors + mods1 + mods2 + customMods;
    # polybar top -l trace (or info) for debugging purposes
    script = ''
      export MONITOR=$(${monitorScript}/bin/monitor)
      echo "Running polybar on $MONITOR"
      export ETH_INTERFACE=$(${networkScript}/bin/check-network eth)
      export WIFI_INTERFACE=$(${networkScript}/bin/check-network wifi)
      echo "Network interfaces $ETH_INTERFACE & $WIFI_INTERFACE"
      polybar top 2>${config.xdg.configHome}/polybar/logs/top.log & disown
      polybar bottom 2>${config.xdg.configHome}/polybar/logs/bottom.log & disown
    '';
  };
}
