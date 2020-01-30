"""""""""""" Basic
" Unicode
set encoding=utf-8

" show number
set number

" highlight current line
set cursorline
hi CursorLine term=bold cterm=bold guibg=Grey40

" turn off annoying beep
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" max line-length mark
" set colorcolumn=120

" show command in NORMAL mode
set showcmd

" 4 soft tabs
let my_tab=4
execute "set shiftwidth=".my_tab
execute "set softtabstop=".my_tab
set expandtab
" allow toggling between local and default mode
function! TabToggle()
  if &expandtab
    set shiftwidth=8
    set softtabstop=0
    set noexpandtab
  else
    execute "set shiftwidth=".g:my_tab
    execute "set softtabstop=".g:my_tab
    set expandtab
  endif
endfunction

" Indent helpers
filetype plugin indent on
set smarttab
" set smartindent

" Stop creating .swp files
set nobackup
set noswapfile

" Set to auto read when a file is changed from the outside
set autoread

" Turn on wildmenu
set wildmenu

" Searching
set incsearch
set hlsearch
set smartcase

" For regular expressions turn magic on
set magic

" Custom file type config
"
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype eruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype yaml setlocal ts=2 sw=2 expandtab
autocmd Filetype lisp setlocal ts=2 sts=2 sw=2 expandtab
au Filetype lisp let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"'}
au Filetype rust let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"', '`':'`'}


""""""""""" Plugins (vim-plug)
call plug#begin('~/.vim/plugged')

" Pair
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/terryma/vim-expand-region'
Plug 'https://github.com/wellle/targets.vim'

" Rainbow parentheses
Plug 'https://github.com/kien/rainbow_parentheses.vim'
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" White space
Plug 'https://github.com/ntpeters/vim-better-whitespace'
autocmd BufEnter * EnableStripWhitespaceOnSave
let g:strip_whitespace_confirm=0

"""""""" Comment
Plug 'https://github.com/tpope/vim-commentary'

" Search
" Incseach
Plug 'https://github.com/haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" show indent line
" Plug 'Yggdroot/indentLine'

" Lightline
Plug 'https://github.com/itchyny/lightline.vim'
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'mode_map': { 'c': 'NORMAL' },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
      \ },
      \ 'component_function': {
      \   'modified': 'LightlineModified',
      \   'readonly': 'LightlineReadonly',
      \   'fugitive': 'LightlineFugitive',
      \   'filename': 'LightlineFilename',
      \   'fileformat': 'LightlineFileformat',
      \   'filetype': 'LightlineFiletype',
      \   'fileencoding': 'LightlineFileencoding',
      \   'mode': 'LightlineMode',
      \ }
      \ }

function! LightlineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '' : ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft == 'unite' ? unite#get_status_string() :
        \  &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let branch = fugitive#head()
    return branch !=# '' ? ' '.branch : ''
  endif
  return ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

" NERD tree
Plug 'https://github.com/scrooloose/nerdtree'
Plug 'https://github.com/jistr/vim-nerdtree-tabs'

map <C-\> :NERDTreeTabsToggle<CR>

" open NERD tree auto when vim starts up on opening a directory
let g:nerdtree_tabs_open_on_console_startup = 2
let g:nerdtree_tabs_autofind = 1

" close vim if only window left open is NERD
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeTabs") && b:NERDTree.isTabTree()) | q | endif

" Display hidden files
let NERDTreeShowHidden=1

""""""""""" Languages
Plug 'sheerun/vim-polyglot'

" disable header folding
let g:vim_markdown_folding_disabled = 1

" do not use conceal feature, the implementation is not so good
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0

" disable math tex conceal feature
let g:tex_conceal = ""
let g:vim_markdown_math = 1

" support front matter of various format
let g:vim_markdown_frontmatter = 1  " for YAML format
let g:vim_markdown_toml_frontmatter = 1  " for TOML format
let g:vim_markdown_json_frontmatter = 1  " for JSON format


""""""" Theme
Plug 'https://github.com/flazz/vim-colorschemes'
Plug 'https://github.com/rafi/awesome-vim-colorschemes'
Plug 'Rigellute/rigel'

call plug#end()

""""""" Custom
" $ touch ~/.custom.vim
"
try
    source ~/.custom.vim
catch E484 " file not found
endtry
