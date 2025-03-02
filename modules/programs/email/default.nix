{ config
, ...
}:
{
  imports = [
    ./private.nix
    ./work.nix
  ];

  programs = {
    mbsync = {
      enable = false;
    };
    himalaya = {
      enable = true;
      settings = {
        downloads-dir = config.xdg.userDirs.download;
      };
    };
  };

  accounts.email = {
    maildirBasePath = "${config.xdg.dataHome}/mail";
  };
}
