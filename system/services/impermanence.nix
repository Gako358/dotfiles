{ config
, lib
, ...
}:
let
  developSpecificDirs = [
    ".cache/bleep"
    ".cache/bloop"
    ".cache/coursier"
    ".cargo"
    ".m2"
    ".npm"
    ".pulumi"
  ];
in
{

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/ssh"
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
      "/var/log"

      # Systemd requires /usr dir to be populated
      # See: https://github.com/nix-community/impermanence/issues/253
      "/usr/systemd-placeholder"
    ];
    users.leif = {
      directories = [
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Projects"
        "Sources"
        {
          directory = ".gnupg";
          mode = "0700";
        }
        {
          directory = ".ssh";
          mode = "0700";
        }
        {
          directory = ".local/share/direnv";
          mode = "0700";
        }
        {
          directory = ".local/share/keyrings";
          mode = "0700";
        }
      ]
      ++ (lib.optionals config.environment.desktop.develop developSpecificDirs);
    };
  };
}
