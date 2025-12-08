{
  time.timeZone = "Europe/Oslo";
  i18n = {
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "nb_NO.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LC_NUMERIC = "nb_NO.UTF-8";
      LC_TIME = "nb_NO.UTF-8";
      LC_MONETARY = "nb_NO.UTF-8";
      LC_ADDRESS = "nb_NO.UTF-8";
      LC_MEASUREMENT = "nb_NO.UTF-8";
      LC_PAPER = "nb_NO.UTF-8";
      LC_TELEPHONE = "nb_NO.UTF-8";
    };
  };

  # Configure the virtual console keymap from the xserver keyboard settings.
  console = {
    useXkbConfig = true;
  };
}
