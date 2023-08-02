-- Lualine
require("lualine").setup({
  opts = function(_, opts)
    local function lsp_name(msg)
      msg = msg or "Inactive"
      local buf_clients = vim.lsp.get_active_clients()
      if next(buf_clients) == nil then
        if type(msg) == "boolean" or #msg == 0 then
          return "Inactive"
        end
        return msg
      end
      local buf_client_names = {}

      for _, client in pairs(buf_clients) do
        if client.name ~= "null-ls" then
          table.insert(buf_client_names, client.name)
        end
      end

      return table.concat(buf_client_names, ", ")
    end

    opts.sections = vim.tbl_deep_extend("force", opts.sections, {
      lualine_x = {
        {
          require("noice").api.status.message.get_hl,
          cond = require("noice").api.status.message.has,
        },
        {
          require("noice").api.status.command.get,
          cond = require("noice").api.status.command.has,
          color = { fg = "#ff9e64" },
        },
        {
          require("noice").api.status.mode.get,
          cond = require("noice").api.status.mode.has,
          color = { fg = "#ff9e64" },
        },
        {
          require("noice").api.status.search.get,
          cond = require("noice").api.status.search.has,
          color = { fg = "#ff9e64" },
        },
        {
          "diff",
          symbols = { added = "+", modified = "~", removed = "-" },
        },
      },
      lualine_y = {
        {
          lsp_name,
          icon = "",
          color = { gui = "none" },
        },
        {
          "progress",
          separator = " ",
          padding = { left = 1, right = 0 },
        },
        { "location", padding = { left = 0, right = 1 } },
      },
    })
  end,
})

-- bufferline
require("bufferline").setup({
  options = {
    numbers = "none", -- | "ordinal" | "buffer_id" | "both" | function({ ordinal, id, lower, raise }): string,
    close_command = "Bdelete! %d", -- can be a string | function, see "Mouse actions"
    right_mouse_command = "Bdelete! %d", -- can be a string | function, see "Mouse actions"
    max_name_length = 30,
    max_prefix_length = 30, -- prefix used when a buffer is de-duplicated
    show_buffer_icons = true,
    show_buffer_close_icons = false,
    show_close_icon = false,
    show_tab_indicators = true,
    separator_style = "thin", -- | "thick" | "thin" | { 'any', 'any' },
    color_icons = false,
    diagnostics = false,
    highlights = {
      buffer_selected = {
        gui = "none",
      },
    },
    offsets = {
      {
        filetype = "neo-tree",
        text = "Neo-tree",
        highlight = "Directory",
        text_align = "left",
      },
      {
        filetype = "Outline",
        text = "Symbols Outline",
        highlight = "TSType",
        text_align = "left",
      },
    },
  },
})

-- add mini indenet line
require("mini.indentscope").setup({
  -- Options which control scope computation
  options = {
    try_as_border = true,
  },
  symbol = "│",
})

-- noice
vim.keymap.set("n", "<leader>nl", function()
  require("noice").cmd("last")
end)
vim.keymap.set("n", "<leader>nt", function()
  require("noice").cmd("telescope")
end)
vim.keymap.set("n", "<leader>nh", function()
  require("noice").cmd("history")
end)
require("noice").setup({
  lsp = {
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
      ["cmp.entry.get_documentation"] = true,
    },
  },
  routes = {
    {
      filter = {
        event = "msg_show",
        any = {
          { find = "%d+L, %d+B" },
          { find = "; after #%d+" },
          { find = "; before #%d+" },
        },
      },
      view = "mini",
    },
  },
  presets = {
    bottom_search = true,
    command_palette = true,
    long_message_to_split = true,
    inc_rename = true,
  },
})

require("notify").setup({
  background_colour = "#000000",
})

require("mini.animate").setup()
