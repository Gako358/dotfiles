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
    && config.users.users.farstrider.hashedPassword != "!";
in
{
  imports = [
    ./_hardware-configuration.nix
  ];

  networking.hostName = "farmadding";
  users.users = {
    merrinx = {
      isNormalUser = true;
      hashedPassword = "!";
      openssh.authorizedKeys.keys = [
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
      hashedPassword = "!";
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
      message = "farmadding installation readiness requires a unique Farstrider password hash";
    }
  ];

  environment = {
    desktop = {
      enable = true;
      windowManager = "kde";
      develop = false;
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
        ".config"
        ".local/share"
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
    extraStoreDisk.enable = false;
    extraSteamDisk.enable = false;
  };

  disko.devices.disk = lib.mkIf (!installation.ready) (lib.mkForce { });
}
