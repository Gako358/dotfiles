{ config
, lib
, ...
}:
with lib;
let
  cfg = config.service.sops;
in
{
  options.service.sops = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable Sops secrets";
    };
  };

  config = mkIf cfg.enable {
    sops = {
      defaultSopsFile = ../../secrets/default.yaml;
      age.keyFile = "${config.users.users.merrinx.home}/.config/sops/age/keys.txt";
      validateSopsFiles = false;
      age = {
        sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      };
    };
  };
}
