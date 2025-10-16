{
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];

  # Configure the virtual console keymap from the xserver keyboard settings.
  console = {
    earlySetup = true;
    useXkbConfig = true;
  };

  # Need to set colemak so that the early setup catches it
  services.xserver = {
    xkb = {
      layout = "us";
      variant = "colemak_dh";
    };
  };
}
