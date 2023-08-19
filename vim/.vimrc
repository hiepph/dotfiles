"
" BASIC:
"

" Unicode
set encoding=utf-8

" show number
set number

" highlight current line
" set cursorline

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

" Searching
set incsearch
set hlsearch
" set smartcase
set ignorecase
" C-l to clear highlight
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" For regular expressions turn magic on
set magic

" Custom file type config
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype eruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype yaml setlocal ts=2 sw=2 expandtab
autocmd Filetype lisp setlocal ts=2 sts=2 sw=2 expandtab
au Filetype lisp let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"'}
au Filetype rust let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"', '`':'`'}

" Increase history
set history=200


"
" FINDING FILES:
"
"
" Search down into subfolders
" provides tab-completion for all file-related tasks
set path+=**

" Display all matching files when tab complete
set wildmenu

" Expand %% to %:h
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'



" FILE BROWSING:
"
" Tweaks for browsing
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_liststyle=3
let g:netrw_localrmdir='rm -r'


"
" STATUS LINE:
"
set laststatus=2

"
" PLUGINS:
"

" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
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

" Mark
Plug 'https://github.com/kshenoy/vim-signature'

" Snipe
Plug 'https://github.com/justinmk/vim-sneak'
let g:sneak#label = 1

" Languages
Plug 'LnL7/vim-nix'

"
" THEME
"
Plug 'https://github.com/flazz/vim-colorschemes'
set background=dark

call plug#end()

silent! so ~/.custom.vim
