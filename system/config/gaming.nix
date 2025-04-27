{ lib
, ...
}: {
  options.environment.gaming = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable gaming mode in NixOs";
    };
  };
}
