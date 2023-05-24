{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./pulse.nix
  ];
  options.sys.audio = {
    server = mkOption {
      type = types.enum [
        "pulse"
        "pipewire"
        "none"
      ];
      default = "none";
      description = "Audio server";
    };
  };
}
