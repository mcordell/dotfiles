return {
  {
    "shaunsingh/nord.nvim",
    lazy = false,    -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
  },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end
  },

  {
    "tpope/vim-fugitive",
    event = "User AstroGitFile"
  },
  {
    "klen/nvim-test",
    event = "VeryLazy",
    config = function()
      require('nvim-test').setup({
        term = "toggleterm"
      })
      require('nvim-test.runners.rspec'):setup {
        command = "bundle exec rspec", -- a command to run the test runner
      }
    end
  },
  {
    "github/copilot.vim",
    event = "VeryLazy",
  },
  {
    "IndianBoy42/tree-sitter-just",
    event = "VeryLazy",
  },
  {
    "mg979/vim-visual-multi",
    event = "VeryLazy"
  }
  -- You can also add new plugins here as well:
  -- Add plugins, the lazy syntax
  -- "andweeb/presence.nvim",
  -- {
  --   "ray-x/lsp_signature.nvim",
  --   event = "BufRead",
  --   config = function()
  --     require("lsp_signature").setup()
  --   end,
  -- },
}
