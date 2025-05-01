{ config
, lib
, ...
}:
let
  storeCfg = config.system.disks.extraStoreDisk;
  steamCfg = config.system.disks.extraSteamDisk;
  extraStoreDevicePath = config.system.disks.extraStoreDevice;
  extraSteamDevicePath = config.system.disks.extraSteamDevice;

  mainNixSubvol =
    if !storeCfg.enable then {
      "/nix" = {
        mountpoint = "/nix";
        mountOptions = [ "noatime" "noacl" "compress=zstd" "ssd" "space_cache=v2" ];
      };
    } else { };

  storeDisk =
    if storeCfg.enable then
      assert extraStoreDevicePath != null; {
        store = {
          type = "disk";
          device = extraStoreDevicePath;
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

  steamDisk =
    if steamCfg.enable then
      assert extraSteamDevicePath != null; {
        steam = {
          type = "disk";
          device = extraSteamDevicePath;
          content = {
            type = "gpt";
            partitions = {
              opt = {
                size = "100%";
                content = {
                  type = "btrfs";
                  extraArgs = [ "-L" "STEAM" ];
                  subvolumes = {
                    "/opt" = {
                      mountpoint = "/opt";
                      mountOptions = [ "noatime" "noacl" "compress=zstd" "ssd" "space_cache=v2" ];
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
  options = {
    system = {
      disks = {
        mainDevice = lib.mkOption {
          type = lib.types.str;
          default = "/dev/nvme0n1";
          description = "The block device path for the main system disk (containing root, boot, etc.).";
        };
        extraStoreDisk.enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable extra /nix store disk";
        };
        extraStoreDevice = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = "/dev/nvme1n1";
          description = "The block device path for the dedicated extra Nix store disk. Set to null if not used.";
        };
        extraSteamDisk.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable extra /steam store disk";
        };
        extraSteamDevice = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = "/dev/nvme0n1";
          description = "The block device path for the dedicated extra steam store disk. Set to null if not used.";
        };
      };
    };
  };

  config = {
    disko.devices.disk = {
      main = {
        type = "disk";
        device = config.system.disks.mainDevice;
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
    } // storeDisk // steamDisk;

    fileSystems."/persist".neededForBoot = true;
  };
}
