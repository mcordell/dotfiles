return {
  settings = {
    Lua = {
      telemetry = {
        enable = false
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim", "hs" }
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
    }
  }
}
