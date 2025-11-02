{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libnfc
    ccid
    # acsccid -- No longer building, need to check
    pcsclite
    pcsc-tools
  ];

  services.pcscd.enable = true;
}
