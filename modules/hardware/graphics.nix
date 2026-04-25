_: {
  flake.nixosModules.hardware-graphics = {
    hardware = {
      graphics = {
        enable = true;
        enable32Bit = true;
      };
    };
  };
}
