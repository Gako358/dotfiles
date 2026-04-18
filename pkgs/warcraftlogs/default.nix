{
  lib,
  appimageTools,
  fetchurl,
}:

let
  version = "9.0.113";
  pname = "warcraftlogs";

  src = fetchurl {
    url = "https://github.com/RPGLogs/Uploaders-archon-lite/releases/download/v${version}/archon-lite-v${version}.AppImage";
    hash = "sha256-zHt5QpBEvz3S3RDHugLl96mrkUoAHv9sJmQuPoDJYRA=";
  };

  appimageContents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    install -m 444 -D "${appimageContents}/Archon App Lite.desktop" "$out/share/applications/${pname}.desktop"
    substituteInPlace "$out/share/applications/${pname}.desktop" \
      --replace-fail 'Exec=AppRun' 'Exec=${pname}' \
      --replace-fail 'Icon=Archon App Lite' 'Icon=${pname}'
    install -m 444 -D "${appimageContents}/usr/share/icons/hicolor/512x512/apps/Archon App Lite.png" \
      "$out/share/icons/hicolor/512x512/apps/${pname}.png"
  '';

  meta = {
    description = "Warcraft Logs combat log uploader for World of Warcraft";
    mainProgram = "warcraftlogs";
    homepage = "https://www.warcraftlogs.com";
    downloadPage = "https://github.com/RPGLogs/Uploaders-archon-lite/releases";
    license = lib.licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
  };
}
