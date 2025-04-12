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

        ".local/share/direnv"
        ".local/share/fish"

        ".cargo"
        ".emacs.d"
        ".m2"
        ".npm"
        ".password-store"
        ".pulumi"
        ".zen"

        ".config/copilot-chat"
        ".config/discord"
        ".config/github-copilot"
        ".config/protonmail"
        ".config/Slack"
        ".config/spotify"
        ".config/sops/age"
        ".config/sops-nix"
      ];
    };
    stateVersion = "24.11";
  };

  systemd.user.startServices = "sd-switch";
  news.display = "silent";
}
