{ osConfig, lib, ... }:
let
  inherit (osConfig.environment) desktop;
in
{
  imports = [ ../../default.nix ];

  # Monitor settings for gnome
  home.file.".config/monitors.xml".text = lib.mkIf (desktop.windowManager == "gnome") ''
    <monitors version="2">
      <configuration>
        <layoutmode>physical</layoutmode>
        <logicalmonitor>
          <x>1080</x>
          <y>0</y>
          <scale>1</scale>
          <primary>yes</primary>
          <monitor>
            <monitorspec>
              <connector>HDMI-1</connector>
              <vendor>AUS</vendor>
              <product>ASUS VP28U</product>
              <serial>0x0000bec6</serial>
            </monitorspec>
            <mode>
              <width>3840</width>
              <height>2160</height>
              <rate>60.000</rate>
            </mode>
          </monitor>
        </logicalmonitor>
        <logicalmonitor>
          <x>0</x>
          <y>240</y>
          <scale>1</scale>
          <transform>
            <rotation>left</rotation>
            <flipped>no</flipped>
          </transform>
          <monitor>
            <monitorspec>
              <connector>DP-2</connector>
              <vendor>AUS</vendor>
              <product>ASUS VG249</product>
              <serial>0x00034efa</serial>
            </monitorspec>
            <mode>
              <width>1920</width>
              <height>1080</height>
              <rate>60.000</rate>
            </mode>
          </monitor>
        </logicalmonitor>
      </configuration>
    </monitors>
  '';
}
