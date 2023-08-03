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
