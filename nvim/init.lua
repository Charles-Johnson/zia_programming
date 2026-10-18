vim.g.mapleader = ' '
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<Leader>ff', builtin.git_files)
vim.keymap.set('n', '<Leader>fg', builtin.live_grep)
vim.keymap.set('n', '<Leader>fs', builtin.grep_string)
vim.keymap.set('n', '<Leader>rr', "<Plug>(coc-rename)")
vim.keymap.set('n', 'gd', '<Plug>(coc-definition)', {silent = true})
vim.keymap.set('n', 'gr', '<Plug>(coc-references)', {silent = true})
vim.keymap.set('n', '<Leader>ac', '<Plug>(coc-codeaction-cursor)', {silent = true, nowait = true})
vim.keymap.set('n', '[g', "<Plug>(coc-diagnostic-prev)", {silent = true})
vim.keymap.set('n', ']g', "<Plug>(coc-diagnostic-next)", {silent = true})
-- Use <c-space> to trigger completion
vim.keymap.set("i", "<c-space>", "coc#refresh()", {silent = true, expr = true})

