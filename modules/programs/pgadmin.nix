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
      userAttoptions = "NULLIF(ARRAY(SELECT o FROM pg_catalog.unnest(att.attoptions) AS o WHERE pg_catalog.left(o, 3) <> 'pg_'), '{}') AS attoptions";
    in
    {
      config = lib.mkIf (desktop.enable && desktop.develop) {
        home.packages = [
          # psycopg 3.3.5 turned the private `_encodings._py_codecs` dict that
          # pgadmin 9.14 mutates into a tuple; rebuild the old dict shape.
          (pkgs.pgadmin4-desktopmode.overridePythonAttrs (old: {
            postPatch =
              (old.postPatch or "")
              + ''
                substituteInPlace web/pgadmin/utils/driver/psycopg3/encoding.py \
                  --replace-fail "from flask import current_app" "from flask import current_app

                if isinstance(psycopg._encodings._py_codecs, tuple):
                    psycopg._encodings._py_codecs = {
                        aliases[0]: v for aliases, v in psycopg._encodings._py_codecs}"
              ''
              # PostgreSQL 18.7/17.12 store an internal pg_attver entry in
              # attoptions; backport pgadmin4 88c3ac2db15f, which nixpkgs applies
              # itself from 9.19 on.
              + lib.optionalString (lib.versionOlder old.version "9.19") ''
                schemas=web/pgadmin/browser/server_groups/servers/databases/schemas
                substituteInPlace $schemas/tables/templates/columns/sql/default/properties.sql \
                  --replace-fail "att.attnotnull, att.attoptions," "att.attnotnull, ${userAttoptions},"
                substituteInPlace $schemas/foreign_tables/templates/foreign_tables/sql/default/get_columns.sql \
                  --replace-fail "INH.inheritedid, att.attoptions," "INH.inheritedid, ${userAttoptions},"
                substituteInPlace $schemas/catalog_objects/columns/templates/catalog_object_column/sql/default/properties.sql \
                  --replace-fail "collnspname , attoptions," "collnspname , ${userAttoptions},"
              '';
          }))
        ];
      };
    };
}
