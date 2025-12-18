{ lib
, ...
}:
{
  options.environment.desktop.develop = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Enable Development toolchain";
  };
}
