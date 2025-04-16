{ config, ... }: {
  sops = {
    defaultSopsFile = ../../secrets/default.yaml;
    age.keyFile = "${config.users.users.merrinx.home}/.config/sops/age/keys.txt";
    validateSopsFiles = false;
    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };
  };
}
