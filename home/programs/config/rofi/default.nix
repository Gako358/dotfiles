{pkgs, ...}: {
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = with pkgs; [rofi-calc rofi-emoji];
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = ./theme.rafi;
  };

  # for rofi-emoji to insert emojis directly
  home.packages = [pkgs.xdotool];
}
