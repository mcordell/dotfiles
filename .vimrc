set nocompatible              " be iMproved, required
filetype off                  " required

"hide buffers with unwritten changes
set hidden

call plug#begin('~/.nvim/plugged')

"Plugin essentials
Plug 'tpope/vim-fugitive'
Plug 'kien/ctrlp.vim'
Plug 'airblade/vim-gitgutter'

Plug 'mattboehm/vim-accordion'
Plug 'mattboehm/vim-unstack'
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
Plug 'tpope/vim-dispatch', { 'for': 'ruby' }

"Plug 'malkomalko/projections.vim'
"Plug 'amiorin/vim-project'

Plug 'majutsushi/tagbar'

Plug 'Valloric/YouCompleteMe'
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'

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

"" :A on lib/foo.rb -> unit/lib/foo_spec.rb
"autocmd User Rails/lib/* let b:rails_alternate = 'spec/lib/' . rails#buffer().name()[0:-4] . '_spec.rb'
"
"" :A on unit/lib/foo_spec.rb -> lib/foo.rb
"autocmd User Rails/spec/lib/* let b:rails_alternate = rails#buffer().name()[5:-9] . '.rb'

"Basic
syntax enable 		"syntax highlighting on

"Next three turns on file type
filetype on
filetype plugin on
filetype indent on

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
:command Vimrc tabe ~/.vimrc
:command Source source ~/.vimrc
:command FixEqual s/\(\S\)\([+=.-]\?=\)\(\S\)/\1 \2 \3/g
:command FixSpace s/\(\S\)\([.]\)\(\S\)/\1 \2 \3/g

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
autocmd Filetype php setlocal ts=4 sts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype gitcommit setlocal spell textwidth=72

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
function Ender()
  let endchar = nr2char(getchar())
  execute "normal \<End>a".endchar
  normal `e
endfunction

function Paster()
	return @+
endfunction

let NERDTreeBookmarksFile=expand("$HOME/.vim-NERDTreeBookmarks")
let NERDTreeShowBookmarks=1

let g:pdv_template_dir = $HOME ."/.vim/bundle/pdv/templates_snip"
nnoremap <space>d :call pdv#DocumentWithSnip()<CR>

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

noremap <space>c :TagbarToggle<CR>

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

let g:speckyBannerKey        = "<C-R>b"
let g:speckyQuoteSwitcherKey = "<C-R>'"
let g:speckyRunRdocKey       = "<C-R>r"
let g:speckySpecSwitcherKey  = "<C-R>x"
let g:speckyRunSpecKey       = "<C-R>s"
let g:speckyRunRdocCmd       = "fri -L -f plain"
let g:speckyRunSpecCmd       = "bundle exec rspec -r ~/.vim/bundle/Specky/ruby/specky_formatter.rb -f SpeckyFormatter"
let g:speckyWindowType       = 2

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
"set term=xterm-256color
set t_Co=256
set fillchars+=stl:\ ,stlnc:\
set termencoding=utf-8
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

"Color scheme
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

nmap <space>hs <Plug>GitGutterStageHunk
nmap <space>hr <Plug>GitGutterRevertHunk

"Close that scratch buffer window that opens on autocompleting
autocmd CompleteDone * pclose

let g:rspec_command = "Dispatch bundle exec rspec {spec}"
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

set backspace=indent,eol,start

runtime macros/matchit.vim
