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
    lazy = false
  },
  {
    "tpope/vim-rhubarb",
    lazy = false
  },
  {
    "klen/nvim-test",
    event = "VeryLazy",
    config = function()
      require('nvim-test').setup({
        term = "toggleterm",
        silent = true,
        termOpts = {
          go_back = true
        }
      })
      require('nvim-test.runners.rspec'):setup {
        command = "bundle exec rspec", -- a command to run the test runner
      }
    end
  },
  {
    "IndianBoy42/tree-sitter-just",
    event = "VeryLazy",
    config = function()
      require("nvim-treesitter.parsers").get_parser_configs().just = {
        install_info = {
          url = "https://github.com/IndianBoy42/tree-sitter-just", -- local path or git repo
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main",
          use_makefile = true -- this may be necessary on MacOS (try if you see compiler errors)
        },
        maintainers = { "@IndianBoy42" },
      }
    end
  },
  {
    "mg979/vim-visual-multi",
    event = "VeryLazy"
  },
  {
    "jackMort/ChatGPT.nvim",
    event = "VeryLazy",
    config = function()
      require("chatgpt").setup(
        {
          api_key_cmd = "op read op://Personal/OpenAIAPI/credential --no-newline",
          actions_paths = { "/Users/michael/.dotfiles/astronvim_user_config/plugins/actions.json" }
        }
      )
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim"
    }
  },
  {
    "AndrewRadev/splitjoin.vim",
    event = "VeryLazy"
  },
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
