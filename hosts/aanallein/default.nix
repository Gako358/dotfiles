{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "aanallein";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialHashedPassword = "$7$CU..../....re.hRJT/dAI5QtAnjQ2or/$nY1JgY1nvyNKZKGsFHnpCJUs9ABZEP5HqbQs04Vqmx4";
      openssh.authorizedKeys.keys = [
      ];
      extraGroups = [ "wheel" "video" "audio" "plugdev" ];
    };
  };

  # Modules loaded
  environment.server.enable = true;
  environment.desktop.enable = false;
}
