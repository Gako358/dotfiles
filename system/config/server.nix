{ lib
, ...
}: {
  options.environment.server = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable server toolchain";
    };
  };
}
