{
  pkgs,
  self,
  ...
}:
{
  programs.merrinx-emacs.enable = true;

  home.packages = [ self.packages.${pkgs.stdenv.hostPlatform.system}.eca ];
}
