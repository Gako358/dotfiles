{ config, ... }: {
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    directories = [
      "Documents"
      "Downloads"
      "Music"
      "Pictures"
      "Projects"
      "Sources"
    ];
  };
}
