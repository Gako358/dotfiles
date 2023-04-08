{ pkgs
, config
, lib
, ...
}:
with lib;
with builtins; let
  cfg = config.programs.vscode;
in
{
  options.programs.vscode.enable = lib.mkEnableOption "vscode";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (vscode-with-extensions.override {
        vscodeExtensions = with vscode-extensions;
          [
            bbenoist.nix
            vscodevim.vim
            github.copilot
            rust-lang.rust-analyzer
            ms-azuretools.vscode-docker
            ms-vscode-remote.remote-ssh
          ]
          ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
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
          ];
      })
    ];
  };
}
