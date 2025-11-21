{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      vimPlugins = super.vimPlugins // {
        coc-nvim = super.vimUtils.buildVimPlugin {
          pname = "coc.nvim";
          version = "2025-06-01";
          src = super.fetchFromGitHub {
            owner = "neoclide";
            repo = "coc.nvim";
            rev = "9f7f280194f70229ec667721373a74d238618220";
            sha256 = "0bmpc1dsd796dmg4ig9i1ijl2a4p9jd1dfmwppmfdlpp9awqp7n3";
          };
          meta.homepage = "https://github.com/neoclide/coc.nvim/";
          meta.hydraPlatforms = [ ];
        };
      };
    })
  ];
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
      smart-splits-nvim
      yankring
      vim-nix
      {
        plugin = vim-startify;
        config = "let g:startify_change_to_vcs_root = 0";
      }
      telescope-manix
      telescope-nvim
      nvim-treesitter
      tender-vim
      lightline-vim
      # https://github.com/echasnovski/mini.nvim/tree/main
      minimap-vim
      mini-nvim
      yazi-nvim
      catppuccin-nvim
      ale
      vim-surround
      vim-repeat
      vim-unimpaired
    ];
    extraPackages = with pkgs; [ code-minimap ];
    extraConfig = ''
      " If you have vim >=8.0 or Neovim >= 0.1.5
      if (has("termguicolors"))
       set termguicolors
      endif

      " For Neovim 0.1.3 and 0.1.4
      let $NVIM_TUI_ENABLE_TRUE_COLOR=1

      " Theme
      syntax enable
      colorscheme catppuccin-frappe
      " set lighline theme inside lightline config
      let g:lightline = {'colorscheme': 'catppuccin'}

      let mapleader = " "
      let maplocalleader = ","
    '';
    extraLuaConfig = ''
      -- line numbers
      vim.opt.numberwidth = 3
      vim.opt.number = true
      vim.opt.relativenumber = true
      vim.opt.ruler = false

      local builtin = require('telescope.builtin')
      vim.keymap.set('n', ';', builtin.find_files, {})
      vim.keymap.set('n', '<leader>g', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>b', builtin.buffers, {})
      vim.keymap.set('n', '<leader>h', builtin.help_tags, {})
      vim.keymap.set('n', '<leader>t', builtin.treesitter, {})
    '' + (lib.strings.concatMapStrings (plugin: ''
      require('${plugin}').setup()
    '') [
      "mini.ai"
      "mini.operators"
      "mini.pairs"
      "mini.bracketed"
      "mini.files"
      "mini.jump2d"
      "mini.pick"
    ]) + ''
      local yazi = require("yazi")
      vim.keymap.set("n", "<leader>-", function()
        yazi.yazi()
      end)
    '' + (builtins.readFile ../config/neovim/smart-splits.lua);
  };
  home.activation.createNvimDirectory =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.local/state/nvim/swap/ || mkdir -p ~/.local/state/nvim/swap/
      chown -R $(whoami) ~/.local/state/nvim/swap/
    '';
}
