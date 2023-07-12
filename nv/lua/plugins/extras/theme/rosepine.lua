local M = {}

function M.setup(colorscheme)
  local colorscheme = (string.match(colorscheme, "light") and "onelight" or "onedark_vivid")
  local colorscheme = "rose-pine"
  local theme = require("rose-pine")
  local opts = {
    options = {
      disable_background = true,
    },
  }
  theme.setup(opts)
  -- vim.cmd("colorscheme rose-pine")
  vim.cmd("colorscheme " .. colorscheme)
  -- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

return M
