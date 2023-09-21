{pkgs, ...}: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      effect-blur = "10x2";
      fade-in = 0.1;
      clock = true;

      font = "JetBrainsMono Nerd Font";
      font-size = 15;

      line-uses-inside = true;
      disable-caps-lock-text = true;
      indicator-caps-lock = true;
      indicator-radius = 40;
      indicator-idle-visible = true;
      indicator-y-position = 1000;

      ring-color = "#3e4451";
      inside-wrong-color = "#e06c75";
      ring-wrong-color = "#e06c75";
      key-hl-color = "#98c379";
      bs-hl-color = "#e06c75";
      ring-ver-color = "#d19a66";
      inside-ver-color = "#d19a66";
      inside-color = "#353b45";
      text-color = "#c8ccd4";
      text-clear-color = "#353b45";
      text-ver-color = "#353b45";
      text-wrong-color = "#353b45";
      text-caps-lock-color = "#c8ccd4";
      inside-clear-color = "#56b6c2";
      ring-clear-color = "#56b6c2";
      inside-caps-lock-color = "#d19a66";
      ring-caps-lock-color = "#3e4451";
      separator-color = "#3e4451";
    };
  };
}
