return {
  -- disabled
  { "akinsho/bufferline.nvim", enabled = false },
  { "stevearc/dressing.nvim", enabled = false },
  { "folke/noice.nvim", enabled = false },
  { "nvim-lualine/lualine.nvim", enabled = false },
  { "MunifTanjim/nui.nvim", enabled = false },
  -- notify customization
  {
    "rcarriga/nvim-notify",
    opts = {
      stages = "slide",
      timeout = 3000,
      render = "compact",
    },
  },
}
