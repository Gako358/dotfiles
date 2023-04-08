{ pkgs
, config
, lib
, ...
}:
with lib;
with builtins; {
  hardware.opengl.driSupport = true;
  hardware.keyboard.zsa.enable = true;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  # hardware.video.hidpi.enable = lib.mkDefault true;
  # Set your system kind (needed for flakes)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
