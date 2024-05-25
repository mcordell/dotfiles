return {
  "jose-elias-alvarez/null-ls.nvim",
  opts = function(_, config)
    -- config variable is the default configuration table for the setup function call
    local null_ls = require "null-ls"

    -- Check supported formatters and linters
    -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
    config.sources = {
      -- Set a formatter
      -- null_ls.builtins.formatting.stylua,
      -- null_ls.builtins.formatting.prettier,
      -- null_ls.builtins.formatting.rubocop.with({
      --   command = "bundle",
      --   args = vim.list_extend(
      --     { "exec", "rubocop" },
      --     null_ls.builtins.formatting.rubocop._opts.args
      --   ),
      -- }),
      -- null_ls.builtins.diagnostics.rubocop.with({
      --   command = "bundle",
      --   args = vim.list_extend(
      --     { "exec", "rubocop" },
      --     null_ls.builtins.diagnostics.rubocop._opts.args
      --   ),
      -- })
    }
    return config -- return final config table
  end,
}
