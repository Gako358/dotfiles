{
  imports = [
    ./_hardware-configuration.nix
  ];

  networking.hostName = "aanallein";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialHashedPassword = "$7$CU..../....re.hRJT/dAI5QtAnjQ2or/$nY1JgY1nvyNKZKGsFHnpCJUs9ABZEP5HqbQs04Vqmx4";
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC9jKHucZd/ms1BCvEEveI/lxTYANd4KShwehtaMiy5OsvhG4qGeFf/WajR0AfxOt6AxNKJGm/4dPZpD5MX+AoxYTP7ZIYDIBBArI5rHTogQbW3nRUwusHhgKQJe+Xa5yp6BuLXRQq9WfRbannMyJUiG8uzFGiyz8NIuEpPCHSmcjwKOWOlgcoXvXBr7f/9fsovxaU8vc0sTe1usBkN1/2kPdwT76UC3+r6MZHzZMbhotACbkNuCGkiiNdz40tklJR5uwkFQW2Fc06c0RJ8+BynjeW/CkPCglKwiVg5owd63GbEYMGktMAmlASg+4jivmbDXX3Movyc+LEStoWoFPispYjvu1ZrpvxuaZJej2f5W+P7UeVOP+X1Ate9QZ87ba/KB22uMmIZpoXk6KMq75wajcpEJyWhMvcZK1RgaHijPlLiMW64o0BYGUCM9a8JLeGe6541m1Qv9moA3+mCgbNec/523Yot3DKVLy4xVJXeXhAmT7Hdv9uE07Beu7oGTojc4upXIFmSnRFiGcz1uBfZD5dN5PRFPtnpQGkzRNqnbTZklAiZxqQVbWJc+1NhfgGP6867QD3zKIbkFbEfZPUqeWwoNmkFQ3KBq2QDT+S91hNR6Cta9krgDESdCvYAwfrUiPEohBJ5DrYC5AbQO4cI8xakPuqEStBktPX3Yf9Wsw== gako.footwork856@passinbox.com"
      ];
      extraGroups = [
        "wheel"
        "video"
        "audio"
        "plugdev"
      ];
    };
  };

  # Modules loaded
  environment.server.enable = true;
  environment.desktop = {
    enable = false;
    windowManager = null;
    develop = true;
  };

  service = {
    tailscale.enable = true;
    sops.enable = false;
    ecaAccess = {
      tailnetHost = null;
      remote.enable = false;
      metrics.enable = false;
    };
  };
}
