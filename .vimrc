"Basic
syntax on 		"syntax highlighting on
set nocompatible 	"Don't try to be compatible with vi

filetype on		"Next three turns on file type
filetype plugin on
filetype indent on

" Strip whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

"show line numbers
set number

"Typo fixes
:command WQ wq
:command Wq wq
:command W w
:command Q q

"Change highlighting to underline
highlight clear SpellBad
highlight SpellBad cterm=underline

"Turn Mouse on
set mouse=a

"Set up Pathogen for plugin management
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

"formatting
set wrap "wrap long lines
set tw=80
set autoindent
set shiftwidth=4
set tabstop=4
set pastetoggle=<F8> "Turn off auto indent for a paste

"Langauge specific formatting
"Python
autocmd FileType python set tabstop=4 | set shiftwidth=4 | set expandtab | set smarttab | set softtabstop=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype php setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab

"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
" Remove trailing whitespaces and ^M chars
"autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
"autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig"

"Html
autocmd FileType html,htmldjango,jinjahtml,eruby,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako source ~/.vim/bundle/closetag/plugin/closetag.vim

"Key Maps
map <C-t> :CommandT <CR>
map <space>t :CommandT <CR>
map <Leader>p :tabprev <CR>
map <space>n :tabnext <CR>
noremap <space>t :NERDTreeToggle <CR>
noremap <space>gs :Gstatus<CR>
noremap <space>gd :Gdiff<CR>
nnoremap \ :Ag<SPACE>

"Command-T specific
"Switch to open in tab by default
let g:CommandTAcceptSelectionMap = '<C-t>'
let g:CommandTAcceptSelectionTabMap = '<CR>'

"Color scheme
let g:solarized_termtrans = 1
colorscheme solarized

"Syntastic specific
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_php_phpcs_args="-n -s --standard=~/monkdev/MonkStandard"

"quick function for adding character at end of line
imap <silent><F2> <Esc>v`^me<Esc>gi<C-o>:call Ender()<CR>
function Ender()
  let endchar = nr2char(getchar())
  execute "normal \<End>a".endchar
  normal `e
endfunction

let NERDTreeBookmarksFile=expand("$HOME/.vim-NERDTreeBookmarks")
let NERDTreeShowBookmarks=1

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
