{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; let
    teams-chromium = makeDesktopItem {
      name = "Teams";
      desktopName = "Teams";
      genericName = "Microsoft Teams";
      exec = ''
        ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --force-dark-mode --enable-features=WebUIDarkMode --app="https://teams.live.com"'';
      icon = "teams";
      categories = ["Network" "InstantMessaging"];
      mimeTypes = ["x-scheme-handler/teams"];
    };
  in [teams-chromium];
}
