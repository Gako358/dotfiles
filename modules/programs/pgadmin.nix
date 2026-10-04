_: {
  flake.homeModules.programs-pgadmin =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (osConfig.environment) desktop;
    in
    {
      config = lib.mkIf (desktop.enable && desktop.develop) {
        home.packages = [
          # psycopg 3.3.5 turned the private `_encodings._py_codecs` dict that
          # pgadmin 9.14 mutates into a tuple; rebuild the old dict shape.
          (pkgs.pgadmin4-desktopmode.overridePythonAttrs (old: {
            postPatch = (old.postPatch or "") + ''
              substituteInPlace web/pgadmin/utils/driver/psycopg3/encoding.py \
                --replace-fail "from flask import current_app" "from flask import current_app

              if isinstance(psycopg._encodings._py_codecs, tuple):
                  psycopg._encodings._py_codecs = {
                      aliases[0]: v for aliases, v in psycopg._encodings._py_codecs}"
            '';
          }))
        ];
      };
    };
}
