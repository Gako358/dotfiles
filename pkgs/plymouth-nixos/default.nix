{
  lib,
  stdenvNoCC,
  imagemagick,
  promptY ? "0.45",
}:

stdenvNoCC.mkDerivation {
  pname = "plymouth-theme-nixos";
  version = "1";

  src = ./.;

  nativeBuildInputs = [ imagemagick ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    magick -size 48x48 xc:none -fill '#CBCBCB' \
      -draw 'circle 23.5,23.5 23.5,0.5' PNG32:bullet.png
    magick -size 2580x450 xc:none -fill '#1A1A1AD9' \
      -draw 'roundrectangle 0,0 2579,449 54,54' PNG32:panel.png

    substituteInPlace nixos.script --replace-fail '@promptY@' '${promptY}'

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    themeDir=$out/share/plymouth/themes/nixos
    install -Dm444 nixos.png -t $themeDir
    install -Dm444 nixos.script -t $themeDir
    install -Dm444 bullet.png -t $themeDir
    install -Dm444 panel.png -t $themeDir

    # The NixOS plymouth module rewrites any /nix/store/*/share/plymouth/themes
    # path in here to the initrd location, so these must stay absolute.
    cat > $themeDir/nixos.plymouth <<EOF
    [Plymouth Theme]
    Name=nixos
    Description=Full-bleed nixos splash with a LUKS passphrase prompt
    ModuleName=script

    [script]
    ImageDir=$themeDir
    ScriptFile=$themeDir/nixos.script
    EOF

    runHook postInstall
  '';

  meta = {
    description = "Plymouth boot splash built around nixos.png";
    platforms = lib.platforms.linux;
  };
}
