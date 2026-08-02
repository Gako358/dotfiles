{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:

# Vendored from nixpkgs, which dropped the theme because it propagated
# gtk-engine-murrine (GTK2 only, removed upstream). The GTK3/GTK4 stylesheets
# don't use the engine, so only GTK2 apps render unthemed.
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "flat-remix-gtk";
  version = "20240730";

  src = fetchFromGitHub {
    owner = "daniruiz";
    repo = "flat-remix-gtk";
    rev = finalAttrs.version;
    hash = "sha256-EWe84bLG14RkCNbHp0S5FbUQ5/Ye/KbCk3gPTsGg9oQ=";
  };

  dontBuild = true;

  makeFlags = [ "PREFIX=$(out)" ];

  meta = {
    description = "GTK application theme inspired by material design";
    homepage = "https://drasite.com/flat-remix-gtk";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.all;
  };
})
