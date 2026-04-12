{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    defaultEditor = false;
    viAlias = false;
    vimAlias = true;
    extraConfig = # vim
      ''
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
        set novisualbell
        set noerrorbells
        set relativenumber
        set number
        set number relativenumber
        set clipboard+=unnamedplus
        let mapleader=" "
        let maplocalleader=" "
        syntax on
        set nowrap
        set nohlsearch
        set incsearch
        set termguicolors
        set t_Co=256
        hi Normal guibg=NONE ctermbg=NONE
        set laststatus=0
        hi! link StatusLine Normal
        hi! link StatusLineNC Normal
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

        nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
        nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
        nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
        nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
      '';

    plugins = with pkgs.vimPlugins; [
      {
        plugin = plenary-nvim;
      }
      {
        plugin = telescope-nvim;
        type = "lua";
        config = # lua
          ''
            require('telescope').setup{
              defaults = {
                -- Default configuration for telescope goes here:
                -- config_key = value,
                mappings = {
                  i = {
                    -- map actions.which_key to <C-h> (default: <C-/>)
                    -- actions.which_key shows the mappings for your picker,
                    -- e.g. git_{create, delete, ...}_branch for the git_branches picker
                    ["<C-h>"] = "which_key"
                  }
                }
              },
              pickers = {
                -- Default configuration for builtin pickers goes here:
                -- picker_name = {
                --   picker_config_key = value,
                --   ...
                -- }
                -- Now the picker_config_key will be applied every time you call this
                -- builtin picker
              },
              extensions = {
                -- Your extension configuration goes here:
                -- extension_name = {
                --   extension_config_key = value,
                -- }
                -- please take a look at the readme of the extension you want to configure
              }
            }
          '';
      }
      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = # lua
          ''
            require('nvim-treesitter.configs').setup {
              highlight = {
                enable = true,
                disable = function(lang)
                  return lang ~= "nix"
                    and lang ~= "markdown"
               and lang ~= "org"
                end,
                additional_vim_regex_highlighting = true,
              },
            }
          '';
      }
    ];
  };
}
