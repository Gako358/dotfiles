let
  installation = import ./_installation.nix;
in
if !installation.ready || installation.device == null then
  throw "farmadding installation requires real hardware config, a unique Farstrider password hash, a maintainer SSH key, and a verified device first"
else
  {
    disko.devices.disk = {
      main = {
        type = "disk";
        inherit (installation) device;
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "512M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot/efi";
                mountOptions = [
                  "defaults"
                  "umask=0077"
                ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted_root";
                content = {
                  type = "btrfs";
                  extraArgs = [
                    "-L"
                    "NIXOS"
                  ];
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = [
                        "noatime"
                        "compress=zstd"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };
                    "/persist" = {
                      mountpoint = "/persist";
                      mountOptions = [
                        "noatime"
                        "compress=zstd"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };
                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = [
                        "noatime"
                        "noacl"
                        "compress=zstd"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };
                    "/swap" = {
                      mountpoint = "/.swapvol";
                      swap.swapfile.size = "32G";
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
    fileSystems."/persist".neededForBoot = true;
  }
