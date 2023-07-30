{pkgs, ...}: {
  fonts.packages = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hack"
        "Iosevka"
        "JetBrainsMono"
        "DejaVuSansMono"
        "UbuntuMono"
        "SourceCodePro"
        "FiraMono"
        "Iosevka"
        "LiberationMono"
        "Noto"
        "RobotoMono"
      ];
    })
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    material-design-icons
  ];
}
