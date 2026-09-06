_: {
  flake.homeModules.services-persist = _: {
    home.persistence."/persist" = {
      # allowOther = true;
      # Home folders handled with system.
      # This is because use of fuse for home-manager persistence is up to 4x slower.
    };
  };
}
