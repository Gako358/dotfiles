{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    libnfc
    ccid
    acsccid
    pcsclite
    pcsctools
  ];

  services.pcscd.enable = true;
}
