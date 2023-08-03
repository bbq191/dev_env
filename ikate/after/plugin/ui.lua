-- add mini indenet line
require("mini.indentscope").setup({
  -- Options which control scope computation
  options = {
    try_as_border = true,
  },
  symbol = "│",
})

require("indent_blankline").setup({
  char = "│",
  space_char_blankline = " ",
})

require("notify").setup({
  background_colour = "#000000",
})

require("mini.animate").setup()
