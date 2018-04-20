" For a paranoia.
" Normally `:set nocp` is not needed, because it is done automatically
" when .vimrc is found.
if &compatible
  " `:set nocp` has many side effects. Therefore this should be done
  " only when 'compatible' is set.
  set nocompatible
endif

packadd minpac

filetype plugin indent on
syntax on
set background=dark
colo gruvbox8_soft

set noshowmode
set nojoinspaces
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

if has('gui_running')
  set guifont=Hack:h11
  set guioptions-=T "toolbar
  set guioptions-=m "menubar
  set guioptions-=r "right scroll
  set guioptions-=L "left scroll
endif

call minpac#init()

call minpac#add('scrooloose/nerdtree')

call minpac#add('vim-airline/vim-airline', {'type': 'opt'})
let g:airline_skip_empty_sections = 1
let g:airline_section_y = ''
let g:airline_powerline_fonts = 1
let airline#extensions#whitespace#enabled = 0

call minpac#add('ryanolsonx/gruvbox-airline-theme', {'type': 'opt'})

call minpac#add('vim-airline/vim-airline-themes', {'type': 'opt'})
let g:airline_theme='gruvbox'

call minpac#add('mattn/emmet-vim')
call minpac#add('lunaru/vim-less')
call minpac#add('skammer/vim-css-color')
call minpac#add('hail2u/vim-css3-syntax')
call minpac#add('SirVer/ultisnips')
call minpac#add('honza/vim-snippets' )
call minpac#add('ianks/vim-tsx')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('sbdchd/neoformat')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-commentary')
call minpac#add('lifepillar/vim-solarized8')
call minpac#add('lifepillar/vim-gruvbox8')
call minpac#add('tpope/vim-fugitive')
call minpac#add('ryanolsonx/typescript-vim')
call minpac#add('Quramy/tsuquyomi')

call minpac#add('ctrlpvim/ctrlp.vim')
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v[\/](.*\.egg-info|venv|node_modules|__pycache__|target|dist)|(\.(swp|ico|git|svn))$'

call minpac#add('ryanolsonx/ctrlp-projects.vim')
let g:ctrlp_extensions = ['projects']

call minpac#add('vim-jp/syntax-vim-ex')
call minpac#add('vim-python/python-syntax')

call minpac#add('airblade/vim-gitgutter', {
  \ 'frozen': 1,
  \ 'depth': 0
  \ })
"\ 'ref': '932ffac',
"\ 'do': function('s:switch_ref')

call minpac#add('k-takata/minpac', {'type': 'opt'})

command! PackUpdate source ~/_vimrc | call minpac#update()
command! PackClean source ~/_vimrc | call minpac#clean()

" enabled optional plugins
packadd! vim-airline
packadd! vim-airline-themes
packadd! gruvbox-airline-theme

" ctrl p settings

" airline settings

" python-syntax settings
let g:python_highlight_all = 1

let mapleader = ' '

nnoremap <leader>g :Gstatus<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>s :source %<CR>
nnoremap <leader>p :CtrlPProjects<CR>
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>c :Neoformat<CR>
nnoremap <leader><leader> :set nohlsearch<CR>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
noremap <c-l> <c-w>l
nmap <leader>tt :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
inoremap jj <esc>

function! SetTypeScriptKeyboardMappings()
  nnoremap <c-]> :TsuDefinition<cr>
  nnoremap <c-T> :TsuGoBack<cr>
  nnoremap <S-f12> :TsuReferences<cr>
  nnoremap <f2> :TsuRenameSymbol<cr>
  nnoremap <leader>h :TsuSignatureHelp<cr>
endfunction

function! SetTitleFromCwd()
  let cwdparts = split(getcwd(), '\')
  let dir = cwdparts[len(cwdparts) - 1]
  let &titlestring = dir.' - '.expand('%t')
endfunction

augroup FileTypeSettings
  au!

  au FileType typescript call SetTypeScriptKeyboardMappings()
  au BufEnter * call SetTitleFromCwd()
augroup END

fun! ColorschemeCustomizations()
  highlight! link typescriptSource Special
  if g:colors_name == 'solarized8'
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

call ColorschemeCustomizations()

function! s:switch_ref(hooktype, name)
  let l:ref = minpac#getpluginfo(a:name).ref
  let l:dir = minpac#getpluginfo(a:name).dir

  let l:change_ref = 'cd ' . l:dir . ' && git reset --hard ' . l:ref

  if has('win32')
    let l:cmd = &shell . ' /C '.l:change_ref
  else
    let l:cmd = ['sh', '-c', l:change_ref]
  endif

  call job_start(l:cmd, {
    \ 'on_stdout': function('s:on_switch_ref_stdout'),
    \ 'on_stderr': function('s:on_switch_ref_stderr'),
    \ 'on_exit': function('s:on_switch_ref_exit')
    \ })
endfunction

function! s:on_switch_ref_stdout(jobid, data, event)
  echom 'stdout'.data
endfunction

function! s:on_switch_ref_stderr(jobid, data, event)
  echom 'stderr'.data
endfunction

function! s:on_switch_ref_exit(jobid, err_code, event)
  echom 'error code'.err_code
endfunction
