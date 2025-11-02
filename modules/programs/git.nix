{ pkgs
, ...
}:
let
  gitConfig = {
    core = {
      editor = "emacsclient";
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
        emacsclient --eval \"\
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
    pull.rebase = true;
    push.default = "upstream";
    push.autoSetupRemote = true;
    rebase = {
      updateRefs = true;
    };
    url = {
      "https://github.com/".insteadOf = "gh:";
      "ssh://git@github.com".pushInsteadOf = "gh:";
    };
    github.user = "gako358";
  };

  rg = "${pkgs.ripgrep}/bin/rg";
in
{
  home.packages = with pkgs; [
    diff-so-fancy
    git-crypt
    hub
    tig
  ];

  programs.git = {
    enable = true;
    settings = gitConfig // {
      alias = {
        amend = "commit --amend -m";
        fixup = "!f(){ git reset --soft HEAD~$${1} && git commit --amend -C HEAD; };f";
        loc = "!f(){ git ls-files | ${rg} \"\\.$${1}\" | xargs wc -l; };f";
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

      user = {
        email = "gako.footwork856@passinbox.com";
        name = "merrinx";
      };
    };

    ignores = [
      "*.bloop"
      "*.bsp"
      "*.metals"
      "*.metals.sbt"
      "*metals.sbt"
      "*.direnv"
      "*.envrc"
      "*hie.yaml"
      "*.mill-version"
      "*.jvmopts"
    ];

    includes = [
      {
        condition = "gitdir:~/Workflow/";
        contents = {
          user = {
            name = "Knut Oien";
            email = "knut.andre.gulseth.oien@hnikt.no";
          };
        };
      }
      {
        condition = "hasconfig:remote.*.url:ssh://git@github.com:HNIKT-Tjenesteutvikling-Systemutvikling/**";
        contents = {
          user = {
            name = "Knut Oien";
            email = "knut.andre.gulseth.oien@hnikt.no";
          };
        };
      }
    ];
  };
}
