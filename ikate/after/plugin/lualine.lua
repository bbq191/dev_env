-- Lualine
local Util = require("user.util")

require("lualine").setup({
  options = {
    theme = "auto",
    globalstatus = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = {
      {
        "diagnostics",
        symbols = { error = "E", warn = "W", info = "I", hint = "H" },
      },
      {
        "filetype",
        icon_only = true,
        separator = "",
        padding = { left = 1, right = 0 },
      },
      {
        "filename",
        path = 1,
        symbols = { modified = "  ", readonly = "", unnamed = "" },
      },
      {
        function()
          return require("nvim-navic").get_location()
        end,
        cond = function()
          return package.loaded["nvim-navic"]
            and require("nvim-navic").is_available()
        end,
      },
    },
    lualine_x = {
      {
        function()
          return require("noice").api.status.command.get()
        end,
        cond = function()
          return package.loaded["noice"]
            and require("noice").api.status.command.has()
        end,
        color = Util.fg("Statement"),
      },
      {
        function()
          return require("noice").api.status.mode.get()
        end,
        cond = function()
          return package.loaded["noice"]
            and require("noice").api.status.mode.has()
        end,
        color = Util.fg("Constant"),
      },
      {
        function()
          return "  " .. require("dap").status()
        end,
        cond = function()
          return package.loaded["dap"] and require("dap").status() ~= ""
        end,
        color = Util.fg("Debug"),
      },
      {
        require("lazy.status").updates,
        cond = require("lazy.status").has_updates,
        color = Util.fg("Special"),
      },
      {
        "diff",
        symbols = { added = "+", modified = "~", removed = "-" },
      },
    },
    lualine_y = {
      {
        "progress",
        separator = " ",
        padding = { left = 1, right = 0 },
      },
      { "location", padding = { left = 0, right = 1 } },
    },
    lualine_z = {
      function()
        return " " .. os.date("%R")
      end,
    },
  },
  extensions = { "neo-tree", "lazy" },
})
