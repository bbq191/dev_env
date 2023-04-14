-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Using Telescope
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.1',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    -- Using RosePine theme
    use({ 'rose-pine/neovim', as = 'rose-pine' })
    vim.cmd('colorscheme rose-pine')

    -- Using treesitter
    use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
    use("nvim-treesitter/nvim-treesitter-context");

    -- Using treesitter-playground
    use('nvim-treesitter/playground')

    -- folke is nvim god
    use({
        "folke/trouble.nvim",
        requires = "nvim-tree/nvim-web-devicons", -- if you do not nees icond comment here
        config = function()
            require("trouble").setup {
                -- icons = false, -- if you do not need icons open here
            }
        end
    })

    -- Using Harpoon
    use('theprimeagen/harpoon')
    use("theprimeagen/refactoring.nvim") -- nice plugin

    use('mbbill/undotree')
    use('tpope/vim-fugitive')

    -- LSP-zero
    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        requires = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' }, -- Required
            {
                -- Optional
                'williamboman/mason.nvim',
                run = function()
                    pcall(vim.cmd, 'MasonUpdate')
                end,
            },
            { 'williamboman/mason-lspconfig.nvim' }, -- Optional

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },         -- Required
            { 'hrsh7th/cmp-buffer' },       -- Optional
            { 'hrsh7th/cmp-path' },         -- Optional
            { 'hrsh7th/cmp-nvim-lsp' },     -- Required
            { 'saadparwaiz1/cmp_luasnip' }, -- Optional
            { 'hrsh7th/cmp-nvim-lua' },     -- Optional

            -- Snippets
            { 'L3MON4D3/LuaSnip' },             -- Required
            { 'rafamadriz/friendly-snippets' }, --Optional
        }
    }

    -- Optional
    -- use("github/copilot.vim") -- i've no money
    use("eandrju/cellular-automaton.nvim") -- really funny
    use("laytan/cloak.nvim")               -- allows you to overlay *'s (or any other character) over defined patterns in defined files.
end)
