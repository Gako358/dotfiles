{ pkgs, lib, ... }:
let
  dc = "${pkgs.docker-compose}/bin/docker-compose";
in
{
  fishAliases = {
    inherit dc;

    # Tool replacements
    cat = "bat";
    du = "${pkgs.ncdu}/bin/ncdu --color dark -rr -x";
    ls = "${pkgs.eza}/bin/eza";
    la = "${lib.getExe pkgs.eza} --long --all --group --header --group-directories-first --sort=type --icons";
    lg = "${lib.getExe pkgs.eza} --long --all --group --header --git";
    lt = "${lib.getExe pkgs.eza} --long --all --group --header --tree --level ";
    ping = "${pkgs.prettyping}/bin/prettyping";
    tree = "${pkgs.eza}/bin/eza -T";
    xdg-open = "${pkgs.mimeo}/bin/mimeo";

    # Navigation
    ".." = "cd ..";

    # Git alias
    gcm = "git checkout master";
    gs = "git status";
    ga = "git add";
    gaa = "git add -A";
    gm = "git commit -m";
    gp = "git push";
    gc = "git checkout";

    # Maven alias
    mvncp = "mvn clean package";
    mvnci = "mvn clean install";

    # Docker
    dps = "${dc} ps";
    dcd = "${dc} down --remove-orphans";
    drm = "docker images -a -q | xargs docker rmi -f";

    # Nix
    nixgc = "nix-collect-garbage";
    nixgcd = "sudo nix-collect-garbage -d";
    update = "nix flake update";
    supdate = "sudo nix flake update";
    upgrade = "sudo nixos-rebuild switch --flake";

    # Locations
    dot = "cd ~/Sources/nixhnikt";
    doc = "cd ~/Documents";
    work = "cd ~/Projects/workspace";
    tod = "cd ~/Projects/workspace/worksetup";
  };
}
