{ config, lib, pkgs, ... }:

{
  programs.yazi = {
    enable = true;
    settings = { tasks = { image_bound = [ 65535 65535 ]; }; };
  };
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    coc = { enable = true; };
    plugins = with pkgs.vimPlugins; [
      {
        plugin = smart-splits-nvim;
        config = (builtins.readFile ../config/neovim/smart-splits.lua);
        type = "lua";
      }
      yankring
      vim-nix
      telescope-manix
      {
        plugin = telescope-nvim;
        config = ''
          local builtin = require('telescope.builtin')
          vim.keymap.set('n', ';', builtin.find_files, {})
          vim.keymap.set('n', '<leader>g', builtin.live_grep, {})
          vim.keymap.set('n', '<leader>b', builtin.buffers, {})
          vim.keymap.set('n', '<leader>h', builtin.help_tags, {})
          vim.keymap.set('n', '<leader>t', builtin.treesitter, {})
        '';
        type = "lua";
      }
      nvim-treesitter
      tender-vim
      lightline-vim
      minimap-vim
      {
        plugin = mini-nvim;
        config = (lib.strings.concatMapStrings (plugin: ''
          require('${plugin}').setup()
        '') [
          "mini.ai" # [[id:04024110-7404-407d-b7f9-9e3817acd9db][mini-ai]]
          "mini.operators"
          "mini.pairs"
          "mini.bracketed"
          "mini.files"
          "mini.pick"
        ]);
        type = "lua";
      }
      {
        plugin = yazi-nvim;
        config = ''
          local yazi = require("yazi")
          vim.keymap.set("n", "<leader>-", function()
            yazi.yazi()
          end)
        '';
        type = "lua";
      }
      {
        plugin = catppuccin-nvim;
        config = ''
          colorscheme catppuccin-frappe
          " set lighline theme inside lightline config
          let g:lightline = {'colorscheme': 'catppuccin'}
        '';
      }
      {
        plugin = vim-startify;
        config = "let g:startify_change_to_vcs_root = 0";
      }
      {
        plugin = ale;
        config = ''
          let g:ale_use_neovim_diagnostics_api = 0
          let g:ale_sign_column_always = 1
        '';
      }
      vim-surround
      vim-repeat
      vim-unimpaired
    ];
    extraPackages = with pkgs; [ code-minimap manix ];
    extraConfig = ''
      " If you have vim >=8.0 or Neovim >= 0.1.5
      if (has("termguicolors"))
       set termguicolors
      endif

      " For Neovim 0.1.3 and 0.1.4
      let $NVIM_TUI_ENABLE_TRUE_COLOR=1

      syntax enable

      let mapleader = " "
      let maplocalleader = ","

      " line numbers
      set numberwidth=3
      set number
      set relativenumber
    '';
  };
  home.activation.createNvimDirectory =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.local/state/nvim/swap/ || mkdir -p ~/.local/state/nvim/swap/
      chown -R $(whoami) ~/.local/state/nvim/swap/
    '';
}
