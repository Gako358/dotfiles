{pkgs, ...}: {
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = ''
        set number relativenumber
        set nobackup

        set autoindent
        set syntax=on
        set termguicolors
        set t_Co=256
        set hidden
      '';
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          copilot-vim
          telescope-nvim
        ];
      };
    };
  };
}
