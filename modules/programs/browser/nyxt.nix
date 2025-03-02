{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nyxt
  ];
}
