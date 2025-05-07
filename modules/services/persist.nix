{ config, ... }: {
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    # Home folders handled with system.
    # This is because use of fuse for home-manager persistence is up to 4x slower.
  };
}
