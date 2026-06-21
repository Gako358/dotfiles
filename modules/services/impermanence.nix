_: {
  flake.nixosModules.services-impermanence =
    {
      config,
      lib,
      ...
    }:
    let
      developSpecificDirs = [
        ".cargo"
        ".m2"
        ".npm"
        ".pulumi"
      ];
    in
    {

      environment.etc."machine-id".source = "/persist/etc/machine-id";

      system.activationScripts.persistMachineId = lib.stringAfter [ "specialfs" ] ''
        if [ ! -s /persist/etc/machine-id ]; then
          mkdir -p /persist/etc
          tr -d '-' < /proc/sys/kernel/random/uuid > /persist/etc/machine-id
        fi
      '';

      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/etc/NetworkManager/system-connections"
          "/etc/ssh"
          {
            directory = "/etc/sops";
            mode = "0700";
          }
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/var/log"

          # Systemd requires /usr dir to be populated
          # See: https://github.com/nix-community/impermanence/issues/253
          "/usr/systemd-placeholder"
        ];
        users.merrinx = {
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
    };
}
