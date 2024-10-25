{
  lib,
  pkgs,
  ...
}: let
  extraCerts = [];

  citrixOverlay = self: super: {
    citrix_workspace_24_05_0 = super.citrix_workspace_24_05_0.overrideAttrs (oldAttrs: rec {
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

  liboggOverlay = final: prev: {
    libogg = prev.libogg.overrideAttrs (prevAttrs: {
      cmakeFlags =
        (
          if prevAttrs ? cmakeFlags
          then prevAttrs.cmakeFlags
          else []
        )
        ++ [
          (final.lib.cmakeBool "BUILD_SHARED_LIBS" true)
        ];
    });
  };
in {
  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [citrixOverlay liboggOverlay];
  home.packages = with pkgs; [
    citrix_workspace_24_05_0
  ];
}
