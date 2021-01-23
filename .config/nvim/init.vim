call plug#begin()

Plug 'scrooloose/nerdtree'
Plug 'morhetz/gruvbox'
Plug 'liuchengxu/vim-which-key'
Plug 'mcchrish/nnn.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

" Only use colorscheme if not in terminal
if exists('g:gnvim')
	set background=light
	colorscheme gruvbox
endif

set hidden
set number 

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
