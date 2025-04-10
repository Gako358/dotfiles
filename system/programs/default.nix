{ pkgs
, ...
}:
{
  imports = [
    ./cachix
    ./desktop.nix
    ./docker.nix
    ./fonts.nix
    ./qemu.nix
    ./shell.nix
    ./xdg.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      wget
      curl
      git
    ];
    persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib"
        "/etc/ssh"
        "/etc/NetworkManager/system-connections"
      ];
      files = [
        "/etc/machine-id"
      ];
      users.merrinx = {
        directories = [
          { directory = ".gnupg"; mode = "0700"; }
          { directory = ".ssh"; mode = "0700"; }
          { directory = ".nixops"; mode = "0700"; }
          { directory = ".local/share/keyrings"; mode = "0700"; }
          { directory = ".pulumi"; mode = "0700"; }

          "Downloads"
          "Music"
          "Pictures"
          "Documents"

          ".local/share/direnv"

          ".cargo"
          ".m2"
          ".npm"

          ".config/discord"
          ".config/protonmail"
          ".config/Slack"
          ".config/spotify"
        ];
        files = [
          ".screenrc"
        ];
      };
    };
  };
}
