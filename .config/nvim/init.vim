call plug#begin()

Plug 'scrooloose/nerdtree'
Plug 'liuchengxu/vim-which-key'
Plug 'mcchrish/nnn.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" Plug 'neovim/nvim-lspconfig'

Plug 'sainnhe/sonokai'
Plug 'morhetz/gruvbox'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

" Only use colorscheme if not in terminal
if exists('g:gnvim') || exists('g:neovide')
    if hostname() == 'corby-desktop'
        colorscheme sonokai 
    else
        set background=light
        colorscheme gruvbox
    endif
endif

if exists('g:gnvim')
    call gnvim#enable_ext_cmdline(0)
endif

set hidden
set number 

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set ignorecase
set smartcase

set guifont=Monospace:h10

let mapleader = " "
noremap <leader>n :NnnPicker<CR>
