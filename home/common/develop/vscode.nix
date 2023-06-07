{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.develop.vscode;
  extensions =
    (with pkgs.vscode-extensions; [
      bbenoist.nix
      kamadorueda.alejandra
      mkhl.direnv
      vscodevim.vim
      eamodio.gitlens
      esbenp.prettier-vscode
      yzhang.markdown-all-in-one
      rust-lang.rust-analyzer
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
    ])
    ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "copilot-nightly";
        publisher = "GitHub";
        version = "1.88.141";
        sha256 = "sIuYqF9f63++7wNSkrBxi5yxMCXz2xlKcpo7hBqxl/w=";
      }
      {
        name = "copilot-chat";
        publisher = "GitHub";
        version = "0.1.2023060501";
        sha256 = "kvN5UCJOqozSA/90COghixLU0dB9VX3M9ZgCTRkkdU0=";
      }
      {
        name = "remote-ssh-edit";
        publisher = "ms-vscode-remote";
        version = "0.47.2";
        sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
      }
      {
        name = "remote-containers";
        publisher = "ms-vscode-remote";
        version = "0.218.0";
        sha256 = "4Li0sYfHOsJMn5FJtvDTGKoGPcRmoosD9tZ7q9H9DfQ=";
      }
      {
        name = "toml";
        publisher = "be5invis";
        version = "0.6.0";
        sha256 = "yk7buEyQIw6aiUizAm+sgalWxUibIuP9crhyBaOjC2E=";
      }
      {
        name = "vscode-clang";
        publisher = "mitaki28";
        version = "0.2.4";
        sha256 = "0sys2h4jvnannlk2q02lprc2ss9nkgh0f0kwa188i7viaprpnx23";
      }
      {
        name = "debug";
        publisher = "webfreak";
        version = "0.25.1";
        sha256 = "1l01sv6kwh8dlv3kygkkd0z9m37hahflzd5bx1wwij5p61jg7np9";
      }
      {
        name = "andromeda";
        publisher = "EliverLara";
        version = "1.6.1";
        sha256 = "uPYESeOcEIPAKW/JiBWI26qG6K9XtEKPYUmG6YVnIgc=";
      }
      {
        name = "vscode-java-pack";
        publisher = "vscjava";
        version = "0.25.2023052400";
        sha256 = "9Fboo740U5MfaPDGMbwmoW+Du8iK2t6547olfmKdvgE=";
      }
      {
        name = "vscode-java-debug";
        publisher = "vscjava";
        version = "0.35.0";
        sha256 = "01sskdm7fizzh6d8bzgdmj9pmrshvh58ks0l6qyf0gr2ifnhli57";
      }
      {
        name = "vscode-maven";
        publisher = "vscjava";
        version = "0.32.2";
        sha256 = "0hn37li6wv5w0m92svr1bmmspwrwcn7k7bm59a58kfgs5j8sccax";
      }
      {
        name = "vscodeintellicode";
        publisher = "VisualStudioExptTeam";
        version = "1.2.14";
        sha256 = "1j72v6grwasqk34m1jy3d6w3fgrw0dnsv7v17wca8baxrvgqsm6g";
      }
      {
        name = "sqltools";
        publisher = "mtxr";
        version = "0.27.1";
        sha256 = "5XhPaxwr0yvIX0wSKDiDm+1iG947s84ULaWpxfpRcAU=";
      }
      {
        name = "vscode-xml";
        publisher = "redhat";
        version = "0.25.2023051904";
        sha256 = "PXux2vqQrV8+nVzLHB1XYP2oKofQNi0jpWTTpfXltHg=";
      }
      {
        name = "markdown-preview-enhanced";
        publisher = "shd101wyy";
        version = "0.6.8";
        sha256 = "9NRaHgtyiZJ0ic6h1B01MWzYhDABAl3Jm2IUPogYWr0=";
      }
    ];
  insiders = (pkgs.vscode.override {isInsiders = true;}).overrideAttrs (old: rec {
    sourceExecutableName = "code-insiders";
    src = pkgs.fetchurl {
      name = "VSCode_insiders.tar.gz";
      url = "https://code.visualstudio.com/sha/download?build=insider&os=linux-x64";
      sha256 = "Q8PSDAfGs8AAQeA/PPAdmVzcz1h0iNqbIZp+1c7by7s=";
    };
    version = "latest";
  });
  code-insiders = pkgs.vscode-with-extensions.override {
    vscode = insiders;
    vscodeExtensions = extensions;
  };
  code = pkgs.vscode-with-extensions.override {
    vscodeExtensions = extensions;
  };
in {
  options.develop.vscode = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable vscode";
    };
  };
  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    programs.vscode = {
      enable = true;
      package = code-insiders;
    };
  };
}
