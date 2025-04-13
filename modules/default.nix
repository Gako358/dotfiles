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
    persistence."/persist/home/merrinx" = {
      allowOther = true;
      directories = [
        "Documents"
        "Downloads"
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
    stateVersion = "24.11";
  };

  systemd.user.startServices = "sd-switch";
  news.display = "silent";
}
