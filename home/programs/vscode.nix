{
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  extensions =
    (with pkgs.vscode-extensions; [
      bbenoist.nix
      vscodevim.vim
      eamodio.gitlens
      kamadorueda.alejandra
      dbaeumer.vscode-eslint
      esbenp.prettier-vscode
      rust-lang.rust-analyzer
      editorconfig.editorconfig
      yzhang.markdown-all-in-one
      ms-azuretools.vscode-docker
    ])
    ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "copilot";
        publisher = "GitHub";
        version = "1.96.263";
        sha256 = "XH76LULYljcmG4Mm3pG2QD4joKoXBHZ9ekbFtoHjt9I=";
      }
      {
        name = "copilot-chat";
        publisher = "GitHub";
        version = "0.1.2023060501";
        sha256 = "kvN5UCJOqozSA/90COghixLU0dB9VX3M9ZgCTRkkdU0=";
      }
      {
        name = "andromeda";
        publisher = "EliverLara";
        version = "1.6.1";
        sha256 = "uPYESeOcEIPAKW/JiBWI26qG6K9XtEKPYUmG6YVnIgc=";
      }
      {
        name = "remote-containers";
        publisher = "ms-vscode-remote";
        version = "0.205.2";
        sha256 = "049l6xm328ij6jzinrznrnnd9fij4rliq9lv5833jh4a5yzv8i69";
      }
      {
        name = "remote-ssh";
        publisher = "ms-vscode-remote";
        version = "0.66.1";
        sha256 = "0qj2ihl74bk1fbixv0g1qzdvaxh4skqww22dyaf17rs6cjf19zps";
      }
      {
        name = "remote-ssh-edit";
        publisher = "ms-vscode-remote";
        version = "0.66.1";
        sha256 = "04sznznvgnp4x7w9mld3j02f9kkx6bxr95knjzbyi1az37bbbmyk";
      }
      {
        name = "remote-wsl";
        publisher = "ms-vscode-remote";
        version = "0.63.0";
        sha256 = "151p7hgffw701fd20zhink7b04zll7by9mg0hp7f3k68w9szngzm";
      }
      {
        name = "vscode-github-actions";
        publisher = "github";
        version = "0.25.7";
        sha256 = "MZrpaWe9PE+S4pRcSxLA417gQL0/oXvnZv+vSrb9nec=";
      }
      {
        name = "debug";
        publisher = "webfreak";
        version = "0.25.1";
        sha256 = "1l01sv6kwh8dlv3kygkkd0z9m37hahflzd5bx1wwij5p61jg7np9";
      }
      {
        name = "python";
        publisher = "ms-python";
        version = "2023.11.11581008";
        sha256 = "JMLGyt8/zlzrqu4nTZZJKcdjp6gEqrLIt6mreDN513U=";
      }
      {
        name = "metals";
        publisher = "scalameta";
        version = "1.23.4";
        sha256 = "WPji2AAfJ6Q0njeuu/QlCui2wu9iUCnZzi0KHV6nCUg=";
      }
      {
        name = "vscode-clang";
        publisher = "mitaki28";
        version = "0.2.4";
        sha256 = "0sys2h4jvnannlk2q02lprc2ss9nkgh0f0kwa188i7viaprpnx23";
      }
      {
        name = "java";
        publisher = "redhat";
        version = "1.20.2023060704";
        sha256 = "rxUtcaFFSZSERmFddZjvdPFJbLDJwgCbWywThVI2bg8=";
      }
      {
        name = "vscode-java-dependency";
        publisher = "vscjava";
        version = "0.18.8";
        sha256 = "1yjzgf96kqm09qlhxpa249fqb2b5wpzw9k53sgr8jx8sfx5qn95b";
      }
      {
        name = "vscode-java-test";
        publisher = "vscjava";
        version = "0.32.0";
        sha256 = "0lq6daz228ipzls88y09zbdsv9n6backs5bddpdam628rs99qvn3";
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
        name = "vscode-java-pack";
        publisher = "vscjava";
        version = "0.25.2023052400";
        sha256 = "9Fboo740U5MfaPDGMbwmoW+Du8iK2t6547olfmKdvgE=";
      }
      {
        name = "vscode-spring-boot-dashboard";
        publisher = "vscjava";
        version = "0.13.2023060100";
        sha256 = "D7wva5+164p4hJzPxvgtGSeU7vU9PRbEEJIAoFbATBk=";
      }
      {
        name = "vscode-spring-initializr";
        publisher = "vscjava";
        version = "0.11.2023041103";
        sha256 = "ZklAaaIEZwEUMj8iXQRbnTUGLS/JXsxVIEVIcHb1b/A=";
      }
      {
        name = "vscode-spring-boot";
        publisher = "vmware";
        version = "1.48.2023062204";
        sha256 = "jiBfquxRsDZlAB+gcPt+/92RzWkNKsf9KYFUUZ3j0QI=";
      }
      {
        name = "vscode-boot-dev-pack";
        publisher = "vmware";
        version = "0.2.1";
        sha256 = "3l9M0wai9aYqZNka7a+AY1YyfYHaiDfd6MYzFmsuG9g=";
      }
      {
        name = "sqltools";
        publisher = "mtxr";
        version = "0.27.1";
        sha256 = "5XhPaxwr0yvIX0wSKDiDm+1iG947s84ULaWpxfpRcAU=";
      }
      {
        name = "nix-ide";
        publisher = "jnoortheen";
        version = "0.1.18";
        sha256 = "1v3j67j8bydyqba20b2wzsfximjnbhknk260zkc0fid1xzzb2sbn";
      }
      {
        name = "nix-env-selector";
        publisher = "arrterian";
        version = "1.0.9";
        sha256 = "TkxqWZ8X+PAonzeXQ+sI9WI+XlqUHll7YyM7N9uErk0=";
      }
      {
        name = "nix-extension-pack";
        publisher = "pinage404";
        version = "3.0.0";
        sha256 = "cWXd6AlyxBroZF+cXZzzWZbYPDuOqwCZIK67cEP5sNk=";
      }
      {
        name = "toml";
        publisher = "be5invis";
        version = "0.6.0";
        sha256 = "yk7buEyQIw6aiUizAm+sgalWxUibIuP9crhyBaOjC2E=";
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
      sha256 = "ndFD6HSZ6sMTgs3V/u7N4dBt6/55GpsHDoo1fO0KgfU=";
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
  programs.vscode = {
    enable = true;
    package = code;
  };
}
