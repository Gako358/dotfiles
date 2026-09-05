{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  installation = import ./_installation.nix;
  usableFarstriderPassword =
    config.users.users.farstrider.hashedPassword != ""
    && config.users.users.farstrider.hashedPassword != "!"
    && config.users.users.farstrider.hashedPassword != config.users.users.merrinx.hashedPassword;
in
{
  imports = [
    ./_hardware-configuration.nix
  ];

  boot.kernelParams = [ "nouveau.noaccel=1" ];

  networking.hostName = "farmadding";
  users.users = {
    merrinx = {
      isNormalUser = true;
      hashedPassword = "!";
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
    farstrider = {
      isNormalUser = true;
      hashedPassword = "$7$CU..../....qkuhViJJTCgCMvVI9/VDA.$PKglcmXWniIiT.tH.dojKAMAXmJmr/9/M6sJE65yplA";
      packages = [ inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default ];
      extraGroups = [
        "networkmanager"
        "video"
        "audio"
        "plugdev"
      ];
    };
  };

  assertions = [
    {
      assertion = !installation.ready || installation.device != null;
      message = "farmadding installation readiness requires a non-null device";
    }
    {
      assertion = !installation.ready || config.users.users.merrinx.openssh.authorizedKeys.keys != [ ];
      message = "farmadding installation readiness requires a maintainer SSH key";
    }
    {
      assertion = !installation.ready || usableFarstriderPassword;
      message = "farmadding installation readiness requires a usable Farstrider password hash";
    }
  ];

  environment = {
    desktop = {
      enable = true;
      windowManager = "kde";
      develop = false;
      kde.persistenceUsers = [ "farstrider" ];
    };
    gaming.enable = false;
    server.enable = false;

    persistence."/persist".users.farstrider = {
      directories = [
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Videos"
        ".zen"
      ];
    };
  };

  service.sops.enable = false;

  services.openssh = {
    openFirewall = false;
    settings = {
      KbdInteractiveAuthentication = false;
      AllowUsers = [ "merrinx" ];
    };
  };
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "none";
  };

  networking.firewall = {
    enable = lib.mkForce true;
    allowedTCPPorts = [ ];
    interfaces.tailscale0.allowedTCPPorts = [ 22 ];
  };

  environment.persistence."/persist".directories = [ "/var/lib/tailscale" ];

  security.sudo.extraRules = [
    {
      users = [ "merrinx" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
    {
      users = [ "farstrider" ];
      commands = [
        {
          command = "${pkgs.tailscale}/bin/tailscale up --operator=farstrider";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  system.disks = {
    mainDevice = lib.mkIf installation.ready installation.device;
    mainDisk.isSolidState = false;
    extraStoreDisk.enable = false;
    extraSteamDisk.enable = false;
  };

  disko.devices.disk = lib.mkIf (!installation.ready) (lib.mkForce { });
}
