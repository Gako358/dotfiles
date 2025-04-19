{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.service.wally;
in
{
  options.service.wally = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Wally";
    };
  };

  config = lib.mkIf cfg.enable {
    services = {
      udev.packages = [
        (pkgs.writeTextFile {
          name = "wally_udev";
          text = ''
            # Rules for Oryx web flashing and live training
            KERNEL=="hidraw*", ATTRS{idVendor}=="16c0", MODE="0664", GROUP="plugdev"
            KERNEL=="hidraw*", ATTRS{idVendor}=="3297", MODE="0664", GROUP="plugdev"

            # Legacy rules for live training over webusb (Not needed for firmware v21+)
              # Rule for all ZSA keyboards
              SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
              # Rule for the Moonlander
              SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
              # Rule for the Ergodox EZ
              SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="1307", GROUP="plugdev"
              # Rule for the Planck EZ
              SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="6060", GROUP="plugdev"

            # Wally Flashing rules for the Ergodox EZ
            ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
            ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
            SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
            KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

            # Keymapp / Wally Flashing rules for the Moonlander and Planck EZ
            SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666", SYMLINK+="stm32_dfu"
            # Keymapp Flashing rules for the Voyager
            SUBSYSTEMS=="usb", ATTRS{idVendor}=="3297", MODE:="0666", SYMLINK+="ignition_dfu"
          '';
          destination = "/etc/udev/rules.d/50-zsa.rules";
        })
      ];
    };
  };
}
