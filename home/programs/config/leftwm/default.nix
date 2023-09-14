{config, ...}: {
  home.file."${config.xdg.configHome}/leftwm".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Sources/dotfiles/home/programs/config/leftwm/config";
}
