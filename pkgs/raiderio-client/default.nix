{
  lib,
  appimageTools,
  fetchurl,
}:

let
  version = "4.10.7";
  pname = "raiderio-client";

  src = fetchurl {
    url = "https://github.com/RaiderIO/raiderio-client-builds/releases/download/v${version}/RaiderIO_Installer_Linux_x86_64.AppImage";
    hash = "sha256-6YkX4DUZLK1F0hP36FGmH3lyDITqjTwyfq9Aqinqi7A=";
  };

  appimageContents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/${pname}.desktop -t $out/share/applications
    substituteInPlace $out/share/applications/${pname}.desktop \
      --replace-fail 'Exec=AppRun' 'Exec=${pname}'
    install -m 444 -D ${appimageContents}/usr/share/icons/hicolor/scalable/${pname}.svg \
      $out/share/icons/hicolor/scalable/apps/${pname}.svg
  '';

  meta = {
    description = "RaiderIO Desktop Client for World of Warcraft Mythic+ and Raid tracking";
    mainProgram = "raiderio-client";
    homepage = "https://raider.io/addon";
    license = lib.licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
  };
}
