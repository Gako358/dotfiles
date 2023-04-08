{ lib
, config
, pkgs
, ...
}: {
  imports = [
    ./audio
    ./core
    ./shell
    ./services
  ];
}
