{
  config,
  pkgs,
  ...
}: let
  discord-chromium = pkgs.makeDesktopItem {
    name = "Discord";
    desktopName = "Discord";
    genericName = "All-in-one cross-platform voice and text chat for gamers";
    exec = ''
      ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --app="https://discord.com/channels/@me"'';
    icon = "discord";
    type = "Application";
    categories = ["Network" "InstantMessaging"];
    terminal = false;
    mimeTypes = ["x-scheme-handler/discord"];
  };
in {
  home.packages = with pkgs; [
    discord-chromium
  ];
}
