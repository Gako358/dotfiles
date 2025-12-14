{ pkgs, inputs, ... }:
{
  environment.systemPackages = [
    inputs.scramgit.defaultPackage.${pkgs.stdenv.hostPlatform.system}
  ];

  # TODO: Need to fix env for python, and shell to run scram
  # systemd.user.services.scramgit = {
  #   description = "Run scramgit to update repositories";
  #   wantedBy = [ "default.target" ];
  #   after = [ "graphical-session.target" ];
  #   serviceConfig = {
  #     Type = "oneshot";
  #     ExecStart = "${inputs.scramgit.defaultPackage.${pkgs.stdenv.hostPlatform.system}}/bin/scramgit";
  #   };
  # };
}
