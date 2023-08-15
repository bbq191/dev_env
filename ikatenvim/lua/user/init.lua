require("user.opt")
require("user.remap")
require("user.lazy")
require("user.util")
require("user.config")

local augroup = vim.api.nvim_create_augroup
local iKateGroup = augroup("iKate", {})

local autocmd = vim.api.nvim_create_autocmd
local yank_group = augroup("HighlightYank", {})

function R(name)
  require("plenary.reload").reload_module(name)
end

autocmd("TextYankPost", {
  group = yank_group,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 40,
    })
  end,
})

autocmd({ "BufWritePre" }, {
  group = iKateGroup,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

local M = {}

---@param opts? LazyVimConfig
function M.setup(opts)
  require("user.config").setup(opts)
end

return M
