-- Mapping data with "desc" stored directly by vim.keymap.set().
--
-- Please use this mappings table to set keyboard mapping since this is the
-- lower level configuration and more robust one. (which-key will
-- automatically pick-up stored data by this setting.)
return {
  -- first key is the mode
  n = {
    -- second key is the lefthand side of the map
    -- mappings seen under group name "Buffer"
    ["<leader>bn"] = { "<cmd>tabnew<cr>", desc = "New tab" },
    ["<leader>bD"] = {
      function()
        require("astronvim.utils.status").heirline.buffer_picker(function(bufnr)
          require("astronvim.utils.buffer").close(
            bufnr)
        end)
      end,
      desc = "Pick to close",
    },
    -- tables with the `name` key will be registered with which-key if it's installed
    -- this is useful for naming menus
    ["<leader>b"] = { name = "Buffers" },
    -- quick save
    ["<leader>fs"] = { ":w!<cr>", desc = "Save File" },
    [",tt"] = { function()
      require("neotest").run.run(vim.fn.expand("%"))
    end, desc = "test file" },
    [",s"] = { function()
      require("neotest").run.run()
    end, desc = "test nearest" },
    [",l"] = { function()
      require("neotest").run.run_last()
    end, desc = "test last" },
    [",ts"] = { function()
      require("neotest").output_panel.toggle()
    end, desc = "test output" },
    [",,"] = { ":b#<cr>", desc = "last buffer" },
  },
  v = {
    ["<leader>pe"] = { function()
      require("chatgpt").edit_with_instructions()
    end,
      desc = "Edit with instructions"
    }
  },
  t = {
    -- setting a mapping to false will disable it
    -- ["<esc>"] = false,
  },
}
