{
  sops = {
    defaultSopsFile = ../../secrets/default.yaml;
    age.keyFile = "/home/merrinx/.config/sops/age/keys.txt";
    validateSopsFiles = false;
    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };
  };
}
