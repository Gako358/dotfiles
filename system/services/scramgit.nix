{ pkgs, inputs, ... }:
{
  environment.systemPackages = [
    inputs.scramgit.defaultPackage.${pkgs.system}
  ];

  systemd.user.services.scramgit = {
    description = "Run scramgit to update repositories";
    wantedBy = [ "default.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${inputs.scramgit.defaultPackage.${pkgs.system}}/bin/scramgit";
    };
  };
}
