{
  lib,
  stdenv,
  fetchurl,
  writeScriptBin,
}: let
  jdtls = stdenv.mkDerivation rec {
    pname = "jdtls";
    version = "1.9.0";

    src = fetchurl {
      url = "https://download.eclipse.org/jdtls/milestones/${version}/jdt-language-server-${version}-202203031534.tar.gz";
      sha256 = "b8af1925cb3b817fd1061e00a45ffbc6aca76819d8b2f5939626009ebf432fc7";
    };
    sourceRoot = ".";

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out
      cp -Rv . $out/

      runHook postInstall
    '';
  };
in
  # Since nvimlsp config does not support jdtls_config outside of JDTLS_HOME,
  # we manually build symlinks and copy config files. (Ideally, this should be
  # changed in nvimlsp but)
  writeScriptBin "jdtls_build_links" ''
    source="${jdtls}"
    target="$1"

    [ -d "$target" ] && echo "Directory \"$target\" exists. Please remove it." && exit 1

    mkdir -p "$target"
    cd "$target"

    for folder in bin plugins features; do
      ln -s "$source/$folder" "$target/$folder"
    done

    cp -r "$source/config_"* "$target"
    chmod +w -R "$target/config_"*

  ''
