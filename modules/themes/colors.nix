_: {
  flake.homeModules.themes-colors = _: {
    colorScheme = {
      slug = "bivrost";
      name = "Bivrost";
      author = "MerrinX";
      palette = import ./_palette.nix;
    };
  };
}
