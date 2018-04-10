set nocompatible

call plug#begin()

" Navigation
Plug 'scrooloose/nerdtree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ryanolsonx/ctrlp-projects.vim'

" Build
Plug 'ryanolsonx/ctrlp-npm.vim'

" Statusline
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'

" Colors
Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'

" Languages
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'lunaru/vim-less'
Plug 'skammer/vim-css-color'
Plug 'hail2u/vim-css3-syntax'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets' 
Plug 'ianks/vim-tsx'

" Environment
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'json'] }
Plug 'editorconfig/editorconfig-vim'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" To add
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'

call plug#end()

set background=dark
color solarized

syntax enable
filetype plugin on

" my defaults
set signcolumn=yes
set wildmenu
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
set incsearch
set ignorecase
set smartcase
set hlsearch
set autoread
set autowrite
set visualbell
set lazyredraw
set scrolloff=2
" TODO: get list from janus
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

" airline settings
let g:airline_skip_empty_sections = 1
let g:airline_section_y = ''
let g:airline_powerline_fonts = 1
let airline#extensions#whitespace#enabled = 0

" solarized settings
let g:solarized_termcolors=256
let g:solarized_underline=0

" key bindings
let mapleader = ' '

inoremap jj <esc>
nnoremap <leader>g :Gstatus<cr>
nnoremap <leader>s :w<cr>
nnoremap <leader>ev :e ~/dotfiles/.vimrc<CR>
nnoremap <leader>ss :source %<CR>
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader><space> :nohlsearch<cr>
nnoremap <leader>f :PrettierAsync<CR>
nnoremap <leader>v :vsp<CR>
nnoremap <leader>h :sp<CR>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
inoremap <C-Space> <c-x><c-o>

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

fun! ColorschemeCustomizations()
  if g:colors_name == 'solarized'
    if &background == 'light'
      highlight CursorLine guibg='#FDF6E3'
      highlight CursorLineNr guibg='#FDF6E3' guifg='#657B83'
      highlight StatusLine guifg='#EFE8D5' guibg='#657B83'
      highlight StatusLineNC guifg='#DDD6C7' guibg='#657B83'
      highlight VertSplit guifg='#FDF6E3' guibg='#FDF6E3'
    else
      highlight CursorLineNr guibg='#063642' guifg='#586E75'
      highlight StatusLine guifg='#063642' guibg='#6B8686'
      highlight StatusLineNC guifg='#042730' guibg='#5F696B'
    endif
  endif
endfu

augroup colorscheme_customizations
  autocmd!
  autocmd ColorScheme * call ColorschemeCustomizations()
augroup END

" status line

let g:statusline_current = 'normal'

function! SL(function)
  if exists('*'.a:function)
    return call(a:function,[])
  else
    return ''
  endif
endfunction

function! NormalStatusline()
  set statusline=\ %t\ %m%r%w%{SL('fugitive#statusline')}\ %y
  let g:statusline_current = 'normal'
endfunction

function! MoreStatusline()
  set statusline=\ %f\ %m%r%w%{SL('fugitive#statusline')}\ %y
  let g:statusline_current = 'more'
endfunction

function! SetStatusline(initial)
  if a:initial
    call NormalStatusline()
  else
    if g:statusline_current == 'normal'
      call MoreStatusline()
    elseif g:statusline_current == 'more'
      call NormalStatusline()
    endif
  endif
endfunction

:call SetStatusline(1)

nnoremap <leader>l :call SetStatusline(0)<cr>

" autocmd

function! SetTitleFromCwd()
  let cwdparts = split(getcwd(), '\')
  let dir = cwdparts[len(cwdparts) - 1]
  let &titlestring = dir.' - '.expand('%t')
endfunction

if has("autocmd")
  augroup FTOptions
    autocmd!
    " Go to Definition
    autocmd FileType typescript map <F12> <c-]>
    " Find References
    autocmd FileType typescript map <F11> :TsuReferences<cr>
    " Rename symbol
    autocmd FileType typescript map <F2> :TsuRenameSymbol<cr>
    " Jump back
    autocmd FileType typescript map <F10> :TsuGoBack<cr>
    " Quick Fix
    autocmd FileType typescript map <F8> :TsuQuickFix<cr>
    " Show method definition by typing <leader>th in normal mode
    autocmd FileType typescript nmap <buffer> <Leader>th : <C-u>echo tsuquyomi#hint()<CR>
    " tell prettier to auto format on save
    "autocmd bufwritepre *.ts,*.tsx,*.json,*.sjs,*.js PrettierAsync
    autocmd BufNewFile,BufRead *.sjs set syntax=javascript
    autocmd BufRead,BufNewFile * setlocal signcolumn=yes
    autocmd FileType nerdtree setlocal signcolumn=no
    autocmd BufEnter * call SetTitleFromCwd()
  augroup END
endif
" --------------------------
" }}}1
