{ lib
, config
, pkgs
, ...
}: {
  imports = [
    ./gnome
    ./xorg
    ./kde
  ];
}
