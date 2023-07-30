{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.citrix;
in {
  options.programs.citrix.enable = mkEnableOption "Citrix Workspace App";

  config = lib.mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = with pkgs; [
      citrix_workspace
    ];
    nixpkgs.overlays = [
      (self: super: let
        extraCerts = [
        ];
      in {
        citrix_workspace = super.citrix_workspace.overrideAttrs (oldAttrs: rec {
          inherit extraCerts;
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
      })
    ];
  };
}
