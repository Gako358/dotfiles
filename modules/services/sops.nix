_: {
  # NixOS side: sops-nix system options + age key
  flake.nixosModules.services-sops =
    {
      config,
      lib,
      ...
    }:
    let
      cfg = config.service.sops;
    in
    {
      options.service.sops = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable Sops secrets";
        };
      };

      config = lib.mkIf cfg.enable {
        sops = {
          defaultSopsFile = ../../secrets/default.yaml;
          validateSopsFiles = false;
          age = {
            # Primary identity: derived from the host's SSH ed25519 host key.
            # This is what makes scripts/install.sh's bootstrap work — seeding
            # this file at /mnt/persist/etc/ssh/ before nixos-install lets
            # sops-nix decrypt secrets on the first activation.
            sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

            # Optional secondary identity: if a private age key is dropped at
            # this path on a running host, sops-nix will use it as an extra
            # decryption identity (e.g. the master/admin key for emergency
            # recovery). This file is NOT used during a fresh install — the
            # user's home directory does not exist yet at that point.
            keyFile = "${config.users.users.merrinx.home}/.config/sops/age/keys.txt";
          };
        };
      };
    };

  # home-manager side: HM sops integration + persistence
  flake.homeModules.services-sops =
    {
      osConfig,
      config,
      lib,
      ...
    }:
    {
      config = lib.mkIf osConfig.service.sops.enable {
        home.persistence."/persist/" = {
          directories = [
            ".config/sops/age"
            ".config/sops-nix"
          ];
        };

        sops = {
          age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
          defaultSopsFile = ../../secrets/default.yaml;
          validateSopsFiles = false;
          secrets = {
            "private_keys/gako" = {
              path = "${config.home.homeDirectory}/.ssh/id_rsa";
            };
            "public_keys/gako" = {
              path = "${config.home.homeDirectory}/.ssh/id_rsa.pub";
            };
          };
        };
      };
    };
}
