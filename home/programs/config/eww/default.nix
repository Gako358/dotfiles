{
  config,
  pkgs,
  ...
}: {
  home.file."${config.xdg.configHome}/eww".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Sources/dotfiles/home/programs/config/eww/eww-bar";

  home.packages = with pkgs; [
    eww
  ];
}
