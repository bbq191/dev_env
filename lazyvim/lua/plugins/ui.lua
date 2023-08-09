return {
  -- disabled
  { "akinsho/bufferline.nvim", enabled = false },
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
