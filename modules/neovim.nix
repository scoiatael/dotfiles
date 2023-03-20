{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    coc = { enable = true; };
    plugins = with pkgs.vimPlugins; [
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
    ];
    extraConfig = ''
      " If you have vim >=8.0 or Neovim >= 0.1.5
      if (has("termguicolors"))
       set termguicolors
      endif

      " For Neovim 0.1.3 and 0.1.4
      let $NVIM_TUI_ENABLE_TRUE_COLOR=1

      " Theme
      syntax enable
      colorscheme tender
      " set lighline theme inside lightline config
      let g:lightline = { 'colorscheme': 'tender' }
    '';
    extraLuaConfig = ''
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
    '';
  };
  home.activation.createNvimDirectory =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.local/state/nvim/swap/ || mkdir ~/.local/state/nvim/swap/
      chown -R $(whoami) ~/.local/state/nvim/swap/
    '';
}
