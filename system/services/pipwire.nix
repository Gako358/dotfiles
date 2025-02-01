{
  services = {
    pulseaudio.enable = false;
    # Enable pipewire
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.extraConfig."wireplumber.profiles".main."monitor.libcamera" = "disabled";
    };
  };
}
