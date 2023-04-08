{
  config,
  nixpkgs,
  overlays,
  inputs,
}:
# See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    inputs.agenix.nixosModules.age
    inputs.nixos-wsl.nixosModules.wsl

    {
      # age = {
      #   identityPaths = ["/home/merrinx/.ssh/id_ed25519"];

      #   secrets.github-token = {
      #     file = ../../secrets/github-token.age;
      #     owner = "gako358";
      #     mode = "0444";
      #   };
      # };

      nix = import ../../nix-settings.nix {
        inherit inputs system nixpkgs;
        max-jobs = 12;
      };

      nixpkgs = {inherit config overlays;};
      networking.hostName = "wsl";
      system.stateVersion = "22.05";

      wsl = {
        enable = true;
        automountPath = "/mnt";
        defaultUser = "merrinx";
        startMenuLaunchers = true;
        wslConf.network.hostname = "wsl";
      };
    }

    ./configuration.nix
  ];

  specialArgs = {inherit inputs system;};
}
