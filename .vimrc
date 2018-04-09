set nocompatible

color gruvbox

syntax enable
filetype plugin on

" my defaults
set nobackup
set noswapfile
set clipboard=unnamed
set shortmess=a
set encoding=utf-8
set hidden
set history=1000
set laststatus=2
set history=1000
set relativenumber
set backspace=indent,eol,start
set tabstop=2
set shiftwidth=2
set expandtab
" end my defaults

if has('gui_running')
  set guifont=Hack:h11:cANSI:qDRAFT
  set guioptions -=T
  set guioptions -=m
  set guioptions -=r
  set guioptions -=l
  set guioptions -=L
  set guioptions -=R
endif

" ctrl p settings
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'

" key bindings
let mapleader = ' '

nnoremap <leader>g :Gstatus<cr>
nnoremap s :w<cr>
