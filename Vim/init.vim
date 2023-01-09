
colorscheme default

set visualbell
syntax on
filetype plugin indent on
set tabstop=4  "number of visual spaces per <tab>"
set softtabstop=4 "number of spaces inserted/removed when editing"
set shiftwidth=4 "how many spaces > and < move"
set noexpandtab "turns <tab> into spaces"
:set lcs+=tab:•·
set incsearch "search as letters are entered"
set hlsearch "highlight matches"
set smartindent
set cindent
set autoindent
set hidden
set noruler
set nowrap
set ignorecase
set smartcase
"set path=$PWD/../**
set path=$PWD/**
let g:netrw_banner=0
set showtabline=0
autocmd TermOpen * startinsert

let mapleader = " "
nnoremap m: :!
noremap U @@

"scroll"
nnoremap J }j
nnoremap K k{j
vnoremap J j}k
vnoremap K k{k

"windows"
nnoremap <c-c> <esc>
nnoremap <c-w>o :tabe %<cr>
tnoremap <c-w>o <c-\><c-o>:tabe %<cr>
nnoremap - 10<c-w>-
nnoremap = 10<c-w>+
nnoremap _ <c-w>j<c-w>c<c-w>p
nnoremap + <c-w>k<c-w>c<c-w>p
tnoremap <c-w> <c-\><c-n><c-w>
tnoremap <c-w><c-l> <c-\><c-n>mL:cf ~/.vim/quickfix.out<cr>
tnoremap <c-w><right> <c-\><c-o>"#p
tnoremap <c-w>c <c-\><c-n><c-w><c-c><c-w>p
tnoremap <c-^> <c-\><c-n><c-^>
nnoremap <c-w>c <c-w><c-c><c-w>p

"search"
inoremap <c-.> <esc>/\V<c-r>-<cr>
vnoremap m "py/<c-r>p<cr><c-o>
nnoremap mm *<c-o>
hi Search term=reverse ctermfg=white ctermbg=red guifg=white guibg=Red

"remap primary keys back"
inoremap <up> <c-p>
inoremap <down> <c-n>
nnoremap <end> <c-e>
nnoremap <home> <c-y>

"line spacing"
nnoremap <space>j mmo<esc>`m
nnoremap <space>k mmO<esc>`m
nnoremap <space>h kJ
nnoremap <space>l i<cr><tab><esc>
nnoremap <space>; i<cr><esc>

"quickfix"
nnoremap go <c-^>
nnoremap ge vEgF
nnoremap <c-j> :cnext<cr>
nnoremap <c-k> :cprev<cr>
nnoremap gr mL"gyiw :!grep -wnrI '<c-r>g' . >~/.vim/quickfix.out<cr>:cf ~/.vim/quickfix.out<cr>
nnoremap gl mL:cf ~/.vim/quickfix.out<cr>

"should get plugin for function motions...
nnoremap yaf va{}Voky
nnoremap daf va{}Vokd
nnoremap vaf va{}Vok

function! ClearTerm()
	:set scrollback=0
	:set scrollback=1000
endfunction

function! GotoTerm(buffername, direction)
    let bnr = bufwinnr(a:buffername)
    if bnr > 0
       :exe bnr . "wincmd w"
    else
       silent execute 'split ' . a:buffername
	   execute 'wincmd ' . a:direction
    endif
endfunction

function! ToggleTerm(buffername, direction)
    let bnr = bufwinnr(a:buffername)
    if bnr > 0
       :exe bnr . "wincmd w"
	   :q
    else
       silent execute 'split ' . a:buffername
	   execute 'wincmd ' . a:direction
	   :normal i
    endif
endfunction

"buffers"
nnoremap <up> :call ToggleTerm('build','K')<cr><c-\><c-n><c-w><c-p>
nnoremap <down> :call GotoTerm('git','J')<cr>iq<c-c><c-l>
nnoremap <right> :F<cr> 
nnoremap <right> <space>:F ~<cr> 
nnoremap <left> :call GotoTerm('build','K')<cr>:call ClearTerm()<cr>i<c-c><c-l><c-p><cr><c-\><c-n><c-w>p
nnoremap <space><left> :call GotoTerm('build','K')<cr>i<c-c><c-\><c-n><c-w>p
nnoremap <c-i> :split ~/notes/<c-r>n.txt<cr>
let @n='dev'

nnoremap g<down> :term<cr><c-\><c-n>:file git<cr><c-^>
nnoremap g<up> :term <cr><c-\><c-n>:file build<cr><c-^>
"create git/build buffers on startup
"

"git"
nnoremap md :b git<cr>:call ClearTerm()<cr>iq<c-c><c-l>git diff <c-\><c-o>"#p<cr>
nnoremap ms :!git status<cr>
nnoremap mf :!git status \| grep modified \| cut -d: -f2 \| tr -d ' ' \| sed 's/$/:0: modified/' >~/.vim/quickfix.out<cr>mL:cf ~/.vim/quickfix.out<cr>
nnoremap mF :!git status \| grep '\t' \| grep '\#' \| grep -v modified \| grep -v ' ' \| cut -d\# -f2 \| tr -d '\t' \| sed 's/$/:0: untracked/' >~/.vim/quickfix.out<cr>mL:cf ~/.vim/quickfix.out<cr>
nnoremap mad :!git add %<cr>
nnoremap mc :!diffix<cr>mL:cf ~/.vim/quickfix.out<cr>
nnoremap mC :!diffix %<cr>mL:cf ~/.vim/quickfix.out<cr>
"should add merge tooling

" comment (should be plugin)
nnoremap ss 1!!comment cpp<cr>
nnoremap s/ !}comment cpp<cr><c-l>
vnoremap / !comment cpp<cr>
nnoremap siw ciw/*<c-r>"*/<esc>
nnoremap suw F/d2lf*d2lb
nnoremap sj 1!!comment cpp<cr>j1!!comment cpp<cr>
nnoremap sk 1!!comment cpp<cr>k1!!comment cpp<cr>
nnoremap d/ j?\/\*<cr>/\*\/<cr><c-o>!n<left>+1<right>comment cpp<cr><c-l>
nnoremap v/ j?\/\*<cr>/\*\/<cr><c-o>vn

"cpp
"should be replaced with surround plugin...
inoremap <c-l> <cr>{<cr>}<esc>O
nnoremap sl ostd::cout << __FUNCTION__ << std::endl;<cr><esc>
"include lowest dir path as well...
nnoremap gh :find %:t:r.h<cr>
nnoremap gp :find %:t:r.cpp<cr>
nnoremap gd "gyiw :!grep -EwnrI 'struct <c-r>g\|class <c-r>g\|enum <c-r>g' . >~/.vim/quickfix.out<cr>:cf ~/.vim/quickfix.out<cr>
" filter out forward declarations
" handle namespace resolutions

"plugins
set rtp+=~/.fzf
noh
