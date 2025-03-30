{ config, pkgs, ... }:
let
  configOrgPath = ./config.org;
  targetLispPath = "${config.xdg.configHome}/nyxt/config.lisp";
in
{
  home.packages = with pkgs; [
    nyxt
  ];

  xdg.configFile."nyxt/config.lisp" = {
    source = configOrgPath;
    onChange = ''
      ${pkgs.emacs}/bin/emacs --batch \
        --eval "(require 'org)" \
        --eval '(org-babel-tangle-file "${configOrgPath}" "${targetLispPath}" "lisp")'
    '';
  };
}
