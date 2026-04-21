{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    defaultEditor = false;
    viAlias = false;
    vimAlias = true;

    extraConfig = ''
      set encoding=utf-8
      set mouse=a

      set tabstop=4
      set shiftwidth=4
      set softtabstop=4
      set expandtab

      set cmdheight=1
      set updatetime=300
      set shortmess+=c
      set tm=500
      set hidden

      set splitbelow
      set splitright
      set signcolumn=yes
      set autoindent

      set noswapfile
      set nobackup
      set nowritebackup
      set noerrorbells
      set novisualbell

      set number relativenumber
      set clipboard+=unnamedplus

      let mapleader=" "
      let maplocalleader=" "

      syntax on
      set nowrap
      set nohlsearch
      set incsearch
      set termguicolors

      hi Normal guibg=NONE ctermbg=NONE
      hi! link StatusLine Normal
      hi! link StatusLineNC Normal
      set laststatus=0
      set statusline=%{repeat('─',winwidth('.'))}

      set list
      command! BufOnly silent! execute "%bd|e#|bd#"

      map <MiddleMouse> <Nop>
      imap <MiddleMouse> <Nop>
      map <2-MiddleMouse> <Nop>
      imap <2-MiddleMouse> <Nop>
      map <3-MiddleMouse> <Nop>
      imap <3-MiddleMouse> <Nop>
      map <4-MiddleMouse> <Nop>
      imap <4-MiddleMouse> <Nop>
    '';

    initLua = ''
      -- Telescope setup
      local telescope = require("telescope")
      local builtin = require("telescope.builtin")

      telescope.setup({
        defaults = {
          mappings = {
            i = {
              ["<C-h>"] = "which_key",
            },
          },
        },
      })

      vim.keymap.set("n", "<leader>ff", builtin.find_files)
      vim.keymap.set("n", "<leader>fg", builtin.live_grep)
      vim.keymap.set("n", "<leader>fb", builtin.buffers)
      vim.keymap.set("n", "<leader>fh", builtin.help_tags)
    '';

    plugins = with pkgs.vimPlugins; [
      plenary-nvim
      telescope-nvim
      # Treesitter without configuration - just syntax highlighting
      nvim-treesitter.withAllGrammars
    ];
  };
}
