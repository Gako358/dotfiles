{ pkgs
, config
, lib
, ...
}:
with lib;
with builtins; {
  options.sys.audio = {
    server = mkOption {
      type = types.enum [
        "pulse"
        "pipewire"
      ];
      default = "pulse";
      description = "Audio server";
    };
  };

  imports = [
    ./pulse.nix
  ];
}
