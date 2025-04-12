{ ... }:
let
  username = "merrinx";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
in
{
  programs = {
    home-manager.enable = true;
    gh.enable = true;
  };
  imports = builtins.concatMap import [
    ./cli
    ./programs
    ./services
    ./themes
  ];
  xdg = {
    inherit configHome;
    enable = true;
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

  home = {
    inherit username homeDirectory;
    persistence."/persist/dotfiles" = {
      removePrefixDirectory = true;
      allowOther = true;
      directories = [
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Projects"
        "Sources"

        ".local/share/direnv"

        ".cargo"
        ".m2"
        ".npm"
        ".pulumi"
        ".zen"

        ".config/discord"
        ".config/protonmail"
        ".config/Slack"
        ".config/spotify"
        ".config/sops"
        ".config/sops-nix"
      ];
    };
    stateVersion = "24.11";
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
