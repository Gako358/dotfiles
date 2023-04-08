{ inputs
, lib
, pkgs
, config
, ...
}:
with lib;
with builtins; {
  environment.systemPackages = with pkgs; [
    (dwm.override {
      conf = ./patches/laptop.def.h;
    })
  ];
}
