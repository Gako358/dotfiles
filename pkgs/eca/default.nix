{
  lib,
  stdenv,
  fetchurl,
  unzip,
  autoPatchelfHook,
  zlib,
}:

let
  version = "0.157.0";
  assets = {
    x86_64-linux = {
      file = "eca-native-static-linux-amd64.zip";
      hash = "sha256-wAjn+g9d3vaIQ6QX7wZ5bCVl668e5nOSVB+i8LXuQFs=";
    };
    aarch64-linux = {
      file = "eca-native-linux-aarch64.zip";
      hash = "sha256-bkOYBP8PQg+guUoi86ZaaQXQXDZ3FvLydVo901+Huas=";
    };
  };
  asset = assets.${stdenv.hostPlatform.system};
in
stdenv.mkDerivation {
  pname = "eca";
  inherit version;

  src = fetchurl {
    url = "https://github.com/editor-code-assistant/eca/releases/download/${version}/${asset.file}";
    inherit (asset) hash;
  };

  nativeBuildInputs = [
    unzip
    autoPatchelfHook
  ];
  buildInputs = [ zlib ];

  dontStrip = true;

  unpackPhase = ''
    runHook preUnpack
    unzip $src
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 eca $out/bin/eca
    runHook postInstall
  '';

  meta = with lib; {
    description = "Editor Code Assistant server (pinned ${version})";
    homepage = "https://github.com/editor-code-assistant/eca";
    license = licenses.asl20;
    platforms = builtins.attrNames assets;
    mainProgram = "eca";
  };
}
