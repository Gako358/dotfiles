{ config, lib, ... }:

with lib;
let

  cfg = config.system.extraStoreDisk;
  mainNixSubvol =
    if !cfg.enable then {
      "/nix" = {
        mountpoint = "/nix";
        mountOptions = [ "noatime" "noacl" "compress=zstd" "ssd" "space_cache=v2" ];
      };
    } else { };

  storeDisk =
    if cfg.enable then {
      store = {
        type = "disk";
        device = "/dev/nvme1n1";
        content = {
          type = "gpt";
          partitions = {
            nix = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted_store";
                content = {
                  type = "btrfs";
                  extraArgs = [ "-L" "STORE" ];
                  subvolumes = {
                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = [ "noatime" "noacl" "compress=zstd" "ssd" "space_cache=v2" ];
                    };
                  };
                };
              };
            };
          };
        };
      };
    } else { };
in

{
  options.system.extraStoreDisk.enable = mkOption {
    type = types.bool;
    default = true;
    description = "Enable extra /nix store disk";
  };

  config = {
    disko.devices.disk = {
      main = {
        type = "disk";
        device = "/dev/nvme0n1";
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
                mountOptions = [ "defaults" "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted_root";
                content = {
                  type = "btrfs";
                  extraArgs = [ "-L" "NIXOS" ];
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = [ "noatime" "compress=zstd" "ssd" "space_cache=v2" ];
                    };
                    "/persist" = {
                      mountpoint = "/persist";
                      mountOptions = [ "noatime" "compress=zstd" "ssd" "space_cache=v2" ];
                    };
                    "/swap" = {
                      mountpoint = "/.swapvol";
                      swap.swapfile.size = "32G";
                    };
                  } // mainNixSubvol;
                };
              };
            };
          };
        };
      };
    } // storeDisk;

    fileSystems."/persist".neededForBoot = true;
  };
}
