# Linux-specific Home Manager configuration
{ config, pkgs, ... }:

{
  # Add Linux-specific packages
  home.packages = with pkgs; [
    # Add Linux-specific packages here
  ];

  # Configure Linux-specific programs
  # programs.firefox.enable = true;
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = true;

    plugins = with pkgs.vimPlugins; [
      plenary-nvim
      vim-fugitive
      nord-vim
      vim-surround
      telescope-nvim
      # Neo-tree dependencies
      nui-nvim
      nvim-web-devicons
      neo-tree-nvim
    ];

    extraConfig = ''
      colorscheme nord
    '';

    extraLuaConfig = ''
      -- Set space as leader key
      vim.g.mapleader = ' '

      -- Keybindings
      vim.keymap.set('n', '<leader>fs', ':w<CR>', { noremap = true, silent = true, desc = 'File save' })
      vim.keymap.set('n', '<leader>ff', ':Telescope find_files<CR>', { noremap = true, silent = true, desc = 'Find files' })
      vim.keymap.set('n', '<leader>gt', ':Telescope git_status<CR>', { noremap = true, silent = true, desc = 'Git status' })
      vim.keymap.set('n', '<leader>ft', ':Neotree toggle<CR>', { noremap = true, silent = true, desc = 'Toggle file tree' })
    '';
  };
}
