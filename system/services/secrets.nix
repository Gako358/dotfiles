{
  sops = {
    defaultSopsFile = ../../secrets/default.yaml;
    validateSopsFiles = false;
    secrets = {
      "email_master-passwd" = {
        mode = "0600";
        owner = "merrinx";
        group = "users";
      };
      "email_home-passwd" = {
        mode = "0600";
        owner = "merrinx";
        group = "users";
      };
      "email_work-passwd" = {
        mode = "0600";
        owner = "merrinx";
        group = "users";
      };
      "email_user" = {
        mode = "0600";
        owner = "merrinx";
        group = "users";
      };
    };

    age = {
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };
  };
}
