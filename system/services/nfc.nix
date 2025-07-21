{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libnfc
    ccid
    # acsccid -- No longer building, need to check
    pcsclite
    pcsctools
  ];

  services.pcscd.enable = true;
}
