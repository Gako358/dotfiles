{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
  extensions =
    (with pkgs.vscode-extensions; [
      bbenoist.nix
      vscodevim.vim
      rust-lang.rust-analyzer
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
    ])
    ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "copilot-nightly";
        publisher = "GitHub";
        version = "1.88.135";
        sha256 = "xDOpr2YyJiVVUMwEHmQqftWKUFEaMx2kCZUhKUWAPHQ=";
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
    ];
  insiders = (pkgs.vscode.override {isInsiders = true;}).overrideAttrs (old: rec {
    sourceExecutableName = "code-insiders";
    src = pkgs.fetchurl {
      name = "VSCode_insiders.tar.gz";
      url = "https://code.visualstudio.com/sha/download?build=insider&os=linux-x64";
      sha256 = "Ywr+vfJL45tXFR50+0qXiR2hOifAp9ZHQELQgEMnZhI=";
    };
  });
  code-insiders = pkgs.vscode-with-extensions.override {
    vscode = insiders;
    vscodeExtensions = extensions;
  };
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    programs.vscode = {
      enable = true;
      package = code-insiders;
    };
  };
}
