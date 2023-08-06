{
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  citrix_workspace_custom = pkgs.citrix_workspace.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.openssl];
    postInstall = ''
      mkdir -p $out/opt/Citrix/ICAClient/keystore/cacerts
      for cert in ${concatStringsSep " " extraCerts}; do
        certName=$(basename "''${cert}" .pem)
        ${pkgs.openssl}/bin/openssl x509 -in "''${cert}" -out "$certName.crt" -outform DER
        cp "''${certName}.crt" $out/opt/Citrix/ICAClient/keystore/cacerts/
      done
    '';
  });

  myCitrixOverlay = self: super: {
    citrix_workspace = citrix_workspace_custom;
  };
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [myCitrixOverlay];

  home.packages = [citrix_workspace_custom];
}
