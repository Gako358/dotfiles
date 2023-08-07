{
  lib,
  pkgs,
  ...
}: let
  extraCerts = [];

  citrixOverlay = self: super: {
    citrix_workspace = super.citrix_workspace.overrideAttrs (oldAttrs: rec {
      inherit extraCerts;
      buildInputs = oldAttrs.buildInputs ++ [self.openssl];
      postInstall = ''
        mkdir -p $out/opt/Citrix/ICAClient/keystore/cacerts
        for cert in ${lib.concatStringsSep " " extraCerts}; do
          certName=$(basename "''${cert}" .pem)
          ${self.openssl}/bin/openssl x509 -in "''${cert}" -out "$certName.crt" -outform DER
          cp "''${certName}.crt" $out/opt/Citrix/ICAClient/keystore/cacerts/
        done
      '';
    });
  };
in {
  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [citrixOverlay];
  home.packages = with pkgs; [
    citrix_workspace
  ];
}
