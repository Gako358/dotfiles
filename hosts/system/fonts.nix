{pkgs, ...}: {
  fonts.packages = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "DejaVuSansMono"
        "FiraCode"
        "FiraMono"
        "Hack"
        "Iosevka"
        "JetBrainsMono"
        "LiberationMono"
        "Noto"
        "RobotoMono"
        "SourceCodePro"
        "UbuntuMono"
      ];
    })
    cascadia-code
    corefonts
    fira-code
    fira-code-symbols
    liberation_ttf
    material-design-icons
    noto-fonts
    noto-fonts-cjk
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-emoji
    noto-fonts-emoji-blob-bin
    noto-fonts-extra
    source-code-pro
    source-sans-pro
    unifont
    vistafonts
    vistafonts-chs
    vistafonts-cht
  ];
}
