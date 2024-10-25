{pkgs, ...}: let
  gitConfig = {
    core = {
      editor = "nvim";
      pager = "diff-so-fancy | less --tabs=4 -RFX";
    };
    init.defaultBranch = "main";
    merge = {
      conflictStyle = "diff3";
      tool = "ediff";
      keepBackup = false;
      trustExitCode = true;
      ediff.keepBackup = false;
      ediff.cmd = ''
        emacs --eval \"\
        (progn\
          (defun ediff-write-merge-buffer ()\
            (let ((file ediff-merge-store-file))\
              (set-buffer ediff-buffer-C)\
              (write-region (point-min) (point-max) file)\
              (message \\\"Merge buffer saved in: %s\\\" file)\
              (set-buffer-modified-p nil)\
              (sit-for 1)))\
          (setq ediff-quit-hook 'kill-emacs\
                ediff-quit-merge-hook 'ediff-write-merge-buffer)\
          (ediff-merge-files-with-ancestor \\\"$LOCAL\\\" \\\"$REMOTE\\\"\
                                           \\\"$BASE\\\" nil \\\"$MERGED\\\"))\"
      '';
    };
    color.ui = true;
    fetch.prune = true;
    pull.rebase = false;
    push.default = "upstream";
    push.autoSetupRemote = true;
    url = {
      "https://github.com/".insteadOf = "gh:";
      "ssh://git@github.com".pushInsteadOf = "gh:";
    };
    github.user = "gako358";
  };

  rg = "${pkgs.ripgrep}/bin/rg";
in {
  home.packages = with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    git-crypt # git files encryption
    hub # github command-line client
    tig # diff and commit view
  ];

  programs.git =
    {
      enable = true;
      aliases = {
        amend = "commit --amend -m";
        fixup = "!f(){ git reset --soft HEAD~\${1} && git commit --amend -C HEAD; };f";
        loc = "!f(){ git ls-files | ${rg} \"\\.\${1}\" | xargs wc -l; };f"; # lines of code
        staash = "stash --all";
        graph = "log --decorate --oneline --graph";
        br = "branch";
        co = "checkout";
        st = "status";
        ls = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate";
        ll = "log --pretty=format:\"%C(yellow)%h%Cred%d\\\\ %Creset%s%Cblue\\\\ [%cn]\" --decorate --numstat";
        cm = "commit -m";
        ca = "commit -am";
        dc = "diff --cached";
      };
      extraConfig = gitConfig;
      ignores = [
        "*.bloop"
        "*.bsp"
        "*.metals"
        "*.metals.sbt"
        "*metals.sbt"
        "*.direnv"
        "*.envrc" # there is lorri, nix-direnv & simple direnv; let people decide
        "*hie.yaml" # ghcide files
        "*.mill-version" # used by metals
        "*.jvmopts" # should be local to every project
      ];
      userEmail = "gako358@outlook.com";
      userName = "merrinx";

      includes = [
        {
          path = "~/Projects/workspace/.gitconfig-work";
          condition = "gitdir:~/Projects/workspace/";
        }
      ];
    }
    // (pkgs.sxm.git or {});
}
