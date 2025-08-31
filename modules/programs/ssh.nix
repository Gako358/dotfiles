{
  programs.ssh = {
    enable = true;

    enableDefaultConfig = false;
    matchBlocks."*" = {
      forwardAgent = false;
      addKeysToAgent = "yes";
      compression = true;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
    };

    matchBlocks = {
      "github.com" = {
        # "Using SSH over the HTTPS port for GitHub"
        # "(port 22 is banned by some proxies / firewalls)"
        hostname = "ssh.github.com";
        port = 443;
        user = "git";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
      };

      "10.0.0.*" = {
        # "allow to securely use local SSH agent to authenticate on the remote machine."
        # "It has the same effect as adding cli option `ssh -A user@host`"
        forwardAgent = true;
      };
    };
  };
}
