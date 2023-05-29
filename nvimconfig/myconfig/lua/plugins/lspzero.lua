return {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
        -- LSP Support
        { 'neovim/nvim-lspconfig' },     -- Required
        {
            -- Optional
            'williamboman/mason.nvim',
            run = function()
                pcall(vim.cmd, 'MasonUpdate')
            end,
        },
        { 'williamboman/mason-lspconfig.nvim' },     -- Optional

        -- Autocompletion
        { 'hrsh7th/nvim-cmp' },             -- Required
        { 'hrsh7th/cmp-buffer' },           -- Optional
        { 'hrsh7th/cmp-path' },             -- Optional
        { 'hrsh7th/cmp-nvim-lsp' },         -- Required
        { 'saadparwaiz1/cmp_luasnip' },     -- Optional
        { 'hrsh7th/cmp-nvim-lua' },         -- Optional

        -- Snippets
        { 'L3MON4D3/LuaSnip' },                 -- Required
        { 'rafamadriz/friendly-snippets' },     --Optional
    }
}
