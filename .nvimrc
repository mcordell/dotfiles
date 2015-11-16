filetype off                  " required

"hide buffers with unwritten changes
set hidden

call plug#begin('~/.nvim/plugged')

"Plugin essentials
Plug 'tpope/vim-fugitive'
Plug 'kien/ctrlp.vim'
Plug 'airblade/vim-gitgutter'

Plug 'editorconfig/editorconfig-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-unimpaired'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-surround'
Plug 'scrooloose/syntastic'
Plug 'docunext/closetag.vim'
Plug 'tpope/vim-repeat'
Plug 'joonty/vim-phpunitqf', { 'for': 'php' }
Plug 'morhetz/gruvbox'
Plug 'janko-m/vim-test'
Plug 'kassio/neoterm'
Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-projectionist'

Plug 'majutsushi/tagbar'
Plug 'jgdavey/vim-blockle', { 'for': 'ruby'}

Plug 'Valloric/YouCompleteMe'
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'

Plug 'mcordell/vim-dispatch'
Plug 'tpope/vim-rake'
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'kevinw/pyflakes-vim', { 'for': 'python' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'mattn/emmet-vim', { 'for': ['html', 'php'] }
Plug 'tobyS/vmustache', { 'for': 'html' }
Plug 'tobyS/pdv', { 'for': 'php' }
Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
Plug 'skwp/vim-spec-finder', { 'for': 'ruby' }
Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }
Plug 'jgdavey/vim-blockle', { 'for': 'ruby' }
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'elzr/vim-json'

Plug 'terryma/vim-multiple-cursors'
Plug 'kshenoy/vim-signature'
Plug 'rizzatti/dash.vim'
Plug 'burke/matcher'
Plug 'gabesoft/vim-ags'

Plug 'bling/vim-airline'


" All of your Plugs must be added before the following line
call plug#end()            " required

filetype plugin indent on    " required


"Basic
syntax enable 		"syntax highlighting on

"Next three turns on file type
filetype on
filetype plugin on
filetype indent on

augroup markdown
    au!
    au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
augroup END

" Strip whitespace on save
autocmd BufWritePre * :%s/\s\+$//e
"show relative line numbers
set relativenumber
set number

"Typo fixes
:command WQ wq
:command Wq wq
:command W w
:command Q q
:command Vimrc tabe ~/.nvimrc
:command Source source ~/.nvimrc

"Change highlighting to underline
highlight clear SpellBad
highlight SpellBad cterm=underline

"Turn Mouse on
set mouse=a


"formatting
set wrap "wrap long lines
set tw=80
set autoindent
set shiftwidth=4
set tabstop=4
set pastetoggle=<F8> "Turn off auto indent for a paste

"Langauge specific formatting
autocmd FileType python set tabstop=4 | set shiftwidth=4 | set expandtab | set smarttab | set softtabstop=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype jade setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype php setlocal ts=4 sts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd Filetype json setlocal ts=2 sts=2 sw=2 expandtab

"Html
autocmd FileType html,htmldjango,jinjahtml,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,jinjahtml,mako source ~/.vim/bundle/closetag.vim/plugin/closetag.vim

"Key Maps

"plugin specific
"map <C-t> :CommandT <CR>
noremap <space>a  :TaskList<CR>
noremap <space>t :CtrlP<CR>

noremap <space>n :NERDTreeToggle <CR>
"noremap <C-t> :NERDTreeToggle <CR>
noremap <space>gs :Gstatus<CR>
noremap <space>gc :Gcommit<CR>
noremap <space>gd :Gdiff<CR>
noremap <space>s  :Ag<SPACE>
noremap <space>j  :Test run unit<CR>
noremap <space>i  :s/\.\([a-z\-]*\)/@include \1/g<CR>
noremap <space>m  :s/\.\([a-z\-]*\)/@mixin \1/g<CR>
noremap <space>v  :s/@/$/g<CR>

vnoremap <C-c> "*y
noremap <C-p> "*p

"movement between buffers
map <space>p :tabprev <CR>
map <space>w :w<CR>
map <space>l :Git! log<CR>gg
nnoremap <C-F> yiw <ESC>:Git commit --fixup=<C-r>"<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Syntastic specific
let g:syntastic_javascript_checkers = ['jshint', 'jscs']
let g:syntastic_php_checkers =  ['php', 'phpcs', 'phpmd']
let g:syntastic_elixir_checkers = ['elixir']
let g:syntastic_ruby_checkers = ['mri', 'rubocop']
let g:syntastic_yaml_checkers = ['jsyaml']
let g:syntastic_enable_elixir_checker  = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_php_phpcs_args="-s --report=csv --standard=.phpcs.xml"
let g:syntastic_javascript_jshint_args = '--config /Users/michael/.jshintrc'
let g:syntastic_javascript_jscs_args = '-c /Users/michael/.jscs.json'
let g:syntastic_always_populate_loc_list = 1
if !exists("g:fugitivediff")
	let g:syntastic_check_on_open = 1
endif
let g:syntastic_check_on_wq = 0

"quick function for adding character at end of line
imap <silent><F2> <Esc>v`^me<Esc>gi<C-o>:call Ender()<CR>
function! Ender()
  let endchar = nr2char(getchar())
  execute "normal \<End>a".endchar
  normal `e
endfunction

function Paster()
	return @+
endfunction

let NERDTreeBookmarksFile=expand("$HOME/.vim-NERDTreeBookmarks")
let NERDTreeShowBookmarks=1

nnoremap <space>d :Dispatch -newbuf ./bin/rspec --format d %<CR>

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

if executable('matcher')
	let g:ctrlp_match_func = { 'match': 'GoodMatch' }

	function! GoodMatch(items, str, limit, mmode, ispath, crfile, regex)

	  " Create a cache file if not yet exists
	  let cachefile = ctrlp#utils#cachedir().'/matcher.cache'
	  if !( filereadable(cachefile) && a:items == readfile(cachefile) )
		call writefile(a:items, cachefile)
	  endif
	  if !filereadable(cachefile)
		return []
	  endif

	  " a:mmode is currently ignored. In the future, we should probably do
	  " something about that. the matcher behaves like "full-line".
	  let cmd = 'matcher --limit '.a:limit.' --manifest '.cachefile.' '
	  if !( exists('g:ctrlp_dotfiles') && g:ctrlp_dotfiles )
		let cmd = cmd.'--no-dotfiles '
	  endif
	  let cmd = cmd.a:str

	  return split(system(cmd), "\n")

	endfunction
end

nnoremap <space>. :CtrlPTag<CR>
nnoremap <space>b :CtrlPBuffer<CR>

"Tagbar
"
let g:tagbar_usearrows = 1

noremap <space>c :Copen<CR>

nnoremap K :Ags -i <C-R><C-W><CR>

" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!

"window opening
set splitright

inoremap <Up> <ESC><C-W><C-K>i
inoremap <Down> <ESC><C-W><C-J>i
inoremap <Left> <Esc>:tabprev<CR>
inoremap <Right> <Esc>:tabnext<CR>

nnoremap <Up> <ESC><C-W><C-K>i
nnoremap <Down> <ESC><C-W><C-J>i
nnoremap <Left> :tabprev<CR>
nnoremap <Right> :tabnext<CR>
nnoremap <space>k :Gdiff forms<CR>

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:snips_author = "Michael Cordell <surpher@gmail.com>"

autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby let g:SuperTabDefaultCompletionType = "context"

"Display stuff
let g:gruvbox_italic=0
set background=dark
colorscheme gruvbox
let g:airline_powerline_fonts = 1
set encoding=utf-8
set fillchars+=stl:\ ,stlnc:\
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

"Color scheme
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

nmap <space>hs <Plug>GitGutterStageHunk
nmap <space>hr <Plug>GitGutterRevertHunk

vmap <space>y "+y
nmap <space>p "+p

"Close that scratch buffer window that opens on autocompleting
autocmd CompleteDone * pclose


"vim-test
let test#ruby#rspec#executable = './bin/rspec'
let test#strategy = "dispatch"
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>t :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>

set backspace=indent,eol,start
runtime macros/matchit.vim

let g:neoterm_position = 'vertical'
let g:neoterm_automap_keys = ',tt'

" run set test lib
nnoremap <silent> ,rt :call neoterm#test#run('all')<cr>
nnoremap <silent> ,rf :call neoterm#test#run('file')<cr>
nnoremap <silent> ,rn :call neoterm#test#run('current')<cr>
nnoremap <silent> ,rr :call neoterm#test#rerun()<cr>

tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

function! Changer()
	norm cs])
	norm ?(
	norm i %i
	norm f(
	norm vf)
endfunction

" changes Ruby symbol hash from [:zzz, :xx] to %i(zzz xx)
function! SymbolArrayToi()
    normal ?[r(/]r)v%:s/\%V[:,]//ggvi%i
endfunction

nnoremap <C-W>O :call MaximizeToggle()<CR>
nnoremap <C-W>o :call MaximizeToggle()<CR>
nnoremap <C-W><C-O> :call MaximizeToggle()<CR>

function! MaximizeToggle()
  if exists("s:maximize_session")
    exec "source " . s:maximize_session
    call delete(s:maximize_session)
    unlet s:maximize_session
    let &hidden=s:maximize_hidden_save
    unlet s:maximize_hidden_save
  else
    let s:maximize_hidden_save = &hidden
    let s:maximize_session = tempname()
    set hidden
    exec "mksession! " . s:maximize_session
    only
  endif
endfunction
