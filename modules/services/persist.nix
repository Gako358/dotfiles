{ config, ... }: {
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    directories = [
      "Documents"
      "Downloads"
      "Gulag"
      "Music"
      "Pictures"
      "Projects"
      "Sources"

      ".cargo"
      ".m2"
      ".npm"
      ".pulumi"

      ".config/copilot-chat"
      ".config/github-copilot"
    ];
  };
}
