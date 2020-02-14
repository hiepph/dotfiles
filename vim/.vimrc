"
" Basic
"
" Unicode
set encoding=utf-8

" show number
set number

" highlight current line
set cursorline
" hi CursorLine term=bold cterm=bold guibg=Grey40

" turn off annoying beep
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

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
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype eruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype yaml setlocal ts=2 sw=2 expandtab
autocmd Filetype lisp setlocal ts=2 sts=2 sw=2 expandtab
au Filetype lisp let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"'}
au Filetype rust let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"', '`':'`'}

" Map 'fd' to escape
imap fd <Esc>



"
" Plugins
"
call plug#begin('~/.vim/plugged')

" Pair
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/terryma/vim-expand-region'

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

" Comment
Plug 'https://github.com/tpope/vim-commentary'

" Incseach
Plug 'https://github.com/haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)


" Languages
Plug 'sheerun/vim-polyglot'

" disable header folding
let g:vim_markdown_folding_disabled = 1

" do not use conceal feature, the implementation is not so good
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0

" support front matter of various format
let g:vim_markdown_frontmatter = 1  " for YAML format
let g:vim_markdown_toml_frontmatter = 1  " for TOML format
let g:vim_markdown_json_frontmatter = 1  " for JSON format


"
" Status line
"


"
" Theme
"
Plug 'https://github.com/flazz/vim-colorschemes'
Plug 'https://github.com/rafi/awesome-vim-colorschemes'

call plug#end()



"
" Custom
"
try
    source ~/.custom.vim
catch E484 " file not found
endtry
