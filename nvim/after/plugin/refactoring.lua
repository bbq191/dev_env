require('refactoring').setup({})
-- FIXME: can't refactor rust
vim.api.nvim_set_keymap("v", "<leader>ri",
    [[ <Esc><Cmd>lua require('refactoring').refactor('Inline Variable')<CR>]],
    { noremap = true, silent = true, expr = false })
