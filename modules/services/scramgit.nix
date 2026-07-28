_: {
  flake.homeModules.services-scramgit =
    {
      config,
      lib,
      pkgs,
      inputs,
      ...
    }:
    let
      owner = "Gako358";

      repos = {
        "101-rs" = "Projects/101-rs";
        ".password-store" = ".password-store";
        agentx = "Sources/agentx";
        archive = "Sources/archive";
        bivrost = "Projects/bivrost";
        books = "Documents/books";
        "bootstrap.nix" = "Projects/bootstrap.nix";
        "borealis.nvim" = "Projects/plugins/borealis.nvim";
        classified = "Documents/classified";
        discologs = "Projects/discologs";
        dotfiles = "Sources/dotfiles";
        dreamer = "Projects/plugins/dreamer";
        dwm = "Projects/suckless/dwm";
        elmugge = "Projects/elmugge";
        emacs-flake = "Projects/emacs-flake";
        flakeUI = "Projects/flakeUI";
        gako358 = "Projects/gako358";
        interviews = "Projects/interviews";
        jamocha = "Projects/wwwsite/jamocha";
        jrt = "Projects/jrt";
        leetcode = "Projects/leetcode";
        Masterplan = "Projects/Masterplan";
        mXoverlay = "Projects/mXoverlay";
        mxgitbucket = "Projects/mxgitbucket";
        neovim = "Projects/neovim";
        notes = "Documents/notes";
        "nvim-chatGPT" = "Projects/plugins/nvim-chatGPT";
        ProjectOS = "Projects/ProjectOS";
        qregAPI = "Projects/wwwsite/openQregX/qregAPI";
        qregData = "Projects/wwwsite/openQregX/qregData";
        qregUI = "Projects/wwwsite/openQregX/qregUI";
        Reports = "Documents/reports";
        research = "Projects/research";
        rockjvm = "Projects/rockjvm";
        rustacean = "Projects/rustacean";
        ruststrom = "Projects/ruststrom";
        schemeQT = "Projects/schemeqt";
        Scram = "Projects/scram";
        "sessions.nvim" = "Projects/plugins/sessions.nvim";
        slock = "Projects/suckless/slock";
        snake = "Projects/snake";
        socrates = "Projects/socrates";
        st = "Projects/suckless/st";
        standup = "Projects/standup";
        vsmugge = "Projects/vsmugge";
      };

      flags = lib.concatLists (
        lib.mapAttrsToList (repo: path: [
          "--repo"
          "git@github.com:${owner}/${repo}.git=${config.home.homeDirectory}/${path}"
        ]) repos
      );

      scramgit = inputs.scramgit.packages.${pkgs.stdenv.hostPlatform.system}.default;
    in
    {
      home.packages = [
        (pkgs.writeShellScriptBin "scramgit" ''
          exec ${lib.getExe scramgit} ${lib.escapeShellArgs flags} "$@"
        '')
      ];
    };
}
