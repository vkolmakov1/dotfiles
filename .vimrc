call plug#begin('~/.vim/plugged')
" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

" Auto-detect indentation
Plug 'tpope/vim-sleuth'

" Adjusts cursor when in edit mode
Plug 'wincent/terminus'

" Powerline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_powerline_fonts = 1

" fzf
" Plug 'junegunn/fzf.vim'
" set rtp+=/usr/local/opt/fzf

" git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Quick file search
Plug 'ctrlpvim/ctrlp.vim'

" emmet (HTML)
Plug 'mattn/emmet-vim'
let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'js',
    \  },
  \}

" rust
Plug 'rust-lang/rust.vim'

" surround plugin for changing quotes and other things
" c-s ' " will change single quotes to double quotes
Plug 'tpope/vim-surround'

Plug 'tpope/vim-commentary'

call plug#end()

" You want Vim, not vi. When Vim finds a vimrc, 'nocompatible' is set anyway.
" We set it explicitely to make our position clear!
set nocompatible

filetype plugin indent on  " Load plugins according to detected filetype.
syntax on                  " Enable syntax highlighting.

set relativenumber

set backspace   =indent,eol,start  " Make backspace work as you would expect.
set hidden                 " Switch between buffers without having to save first.
set laststatus  =2         " Always show statusline.
set display     =lastline  " Show as much as possible of the last line.

set showcmd                " Show already typed keys when more are expected.
set noshowmode             " Don't show default mode

set incsearch              " Highlight while searching with / or ?.
set hlsearch               " Keep matches highlighted.
set ignorecase             " Required to be set for smartcase search
set smartcase              " Use smartcase search

set ttyfast                " Faster redrawing.
set lazyredraw             " Only redraw when necessary.

set splitbelow             " Open new windows below the current window.
set splitright             " Open new windows right of the current window.

set wrapscan               " Searches wrap around end-of-file.
set report      =0         " Always report changed lines.
set synmaxcol   =200       " Only highlight the first 200 columns.

set list                   " Show non-printable characters.
if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
endif

" Disable backup/swap/undo files
set nobackup
set noswapfile
set noundofile
set viminfo     ='100,n$HOME/.vim/files/info/viminfo

set clipboard=unnamed " Use system clipboard

set belloff=all

colorscheme apprentice

" Keybindings
" search files
nnoremap <F1> :CtrlP<CR>

set guifont=Fira_Code:h18
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
set renderoptions=type:directx
set encoding=utf-8
