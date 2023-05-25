{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  weechatSetup = pkgs.weechat.override {
    configure = {availablePlugins, ...}: {
      scripts = with pkgs.weechatScripts; [
        wee-slack
      ];
      init = ''
        /set irc.look.smart_filter on
        /filter add irc_smart * irc_smart_filter *
        /set irc.look.server_buffer independent
        /mouse enable
        /set irc.look.color_nicks_in_nicklist on
        /set weechat.look.color_nick_offline yes
        /set weechat.look.prefix_same_nick " "
        /set weechat.color.chat_nick_colors "1,2,3,4,6,7,9,10,11,12,13,14,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,182,183,184,244,225,226,227
        /set weechat.color.chat_highlight *16
        /set weechat.color.chat_highlight_bg 9
        /set irc.ctcp.clientinfo ""
        /set irc.ctcp.finger ""
        /set irc.ctcp.source ""
        /set irc.ctcp.time ""
        /set irc.ctcp.userinfo ""
        /set irc.ctcp.version ""
        /set irc.ctcp.ping ""

        /plugin unload xfer
        /server add libera irc.libera.chat
        /set irc.server.libera.addresses "irc.libera.chat/6697"
        /set irc.server.libera.ssl on
        /set irc.server.libera.nicks "merrinx,Merrinx,MerrinX"
        /set irc.server.libera.username "merrinx"
        /set irc.server.libera.autoconnect on
        /set irc.server.libera.autojoin "#python,#nixos,#gentoo,#c,##rust,#neovim,#math"
        /set script.scripts.download_enabled on
        /script install autosort.py
        /script install emoji.lua
        /script install colorize_lines.pl
        plugins.var.perl.colorize_lines.hightlight on
        /key bind meta-j /go
      '';
    };
  };
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    home.packages = [
      weechatSetup
    ];
  };
}
