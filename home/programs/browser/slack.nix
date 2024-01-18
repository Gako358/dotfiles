{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; let
    slack-chromium = makeDesktopItem {
      name = "Slack";
      desktopName = "Slack";
      genericName = "One platform for your team and your work";
      exec = "${config.programs.chromium.package}/bin/chromium --app=\"https://app.slack.com/client/T04MZPW21RA/C04MUBWKREZ\"";
      icon = "slack";
      type = "Application";
      terminal = false;
    };
  in [slack-chromium];
}
