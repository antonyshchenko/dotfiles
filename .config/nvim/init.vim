let mapleader = ','

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround' "{{{
  let g:surround_no_insert_mappings = 1
"}}}
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-eunuch' "{{{
  nmap <leader>D :Remove
"}}}
Plug 'junegunn/fzf.vim' 
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install --all' } "{{{
  map <leader><leader> :FZF<CR>
  nnoremap gb :Buffers<cr>
  nnoremap <leader>l :BLines<cr>
  nnoremap <leader>L :Lines<cr>
  nnoremap <leader>t :BTags<cr>
  nnoremap <leader>T :Tags<cr>
"}}}
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim' "{{{
  let g:unite_data_directory='~/.nvim/.cache/unite'
  let g:unite_source_history_yank_enable=1
  let g:unite_prompt='» '
  let g:unite_source_rec_async_command =['ag', '--follow', '--nocolor', '--nogroup','--hidden', '-g', '', '--ignore', '.git', '--ignore', '*.png', '--ignore', 'lib']

  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup --hidden -a -S'
  let g:unite_source_grep_recursive_opt=''

  nnoremap <silent> <c-p> :Unite -auto-resize -start-insert -direction=botright buffer file_rec/async<CR>

	" Open Unite with word under cursor or selection
	nnoremap <silent> <Leader>f :UniteWithCursorWord file_rec/async -profile-name=navigate<CR>

  nnoremap <silent><Leader>g :Unite -auto-resize -start-insert -silent -no-quit -direction=botright grep<CR>
  nnoremap <silent><Leader>G :UniteWithCursorWord -auto-resize -start-insert -silent -no-quit -direction=botright grep<CR>
  nnoremap <leader>qu :UniteClose<CR>

  " Custom mappings for the unite buffer
  autocmd FileType unite call s:unite_settings()

  function! s:unite_settings() "{{{
	  " Enable navigation with control-j and control-k in insert mode
	  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
	  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
    imap <silent><buffer><expr> <C-s>     unite#do_action('split')
    imap <silent><buffer><expr> <C-v>     unite#do_action('vsplit')
    imap <silent><buffer><expr> <C-t>     unite#do_action('tabswitch')
  endfunction "}}}
"}}}

Plug 'vim-airline/vim-airline' "{{{
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#tagbar#enabled = 0
  let g:airline_powerline_fonts = 1
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = '|'
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = '|'
  let g:airline#extensions#whitespace#enabled = 0
  let g:airline#extensions#tabline#show_buffers = 0
  let g:airline#extensions#tabline#show_splits = 0
  let g:airline#extensions#tabline#show_close_button = 0
  let g:airline#extensions#tabline#tab_nr_type = 1
  let g:airline#extensions#tabline#show_tab_nr = 1
  let g:airline#extensions#tabline#show_tab_type = 0
  let g:airline#extensions#bufferline#enabled = 0
  let g:airline#extensions#taboo#enabled = 1
  nmap <leader>1 <Plug>AirlineSelectTab1
  nmap <leader>2 <Plug>AirlineSelectTab2
  nmap <leader>3 <Plug>AirlineSelectTab3
  nmap <leader>4 <Plug>AirlineSelectTab4
  nmap <leader>5 <Plug>AirlineSelectTab5
  nmap <leader>6 <Plug>AirlineSelectTab6
  nmap <leader>7 <Plug>AirlineSelectTab7
  nmap <leader>8 <Plug>AirlineSelectTab8
  nmap <leader>9 <Plug>AirlineSelectTab9
  let g:airline_mode_map = {
      \ '__' : '-',
      \ 'n'  : 'N',
      \ 'i'  : 'I',
      \ 'R'  : 'R',
      \ 'c'  : 'C',
      \ 'v'  : 'V',
      \ 'V'  : 'V',
      \ '' : 'V',
      \ 's'  : 'S',
      \ 'S'  : 'S',
      \ '' : 'S',
      \ }
"}}}
Plug 'vim-airline/vim-airline-themes' "{{{
  let g:airline_theme = 'simple'
"}}}
Plug 'gcmt/taboo.vim' "{{{
  set sessionoptions+=tabpages,globals
  let g:taboo_tabline = 0
  nmap <leader>tr :TabooRename<space>
"}}}

Plug 'easymotion/vim-easymotion' "{{{
  let g:EasyMotion_do_mapping = 0
  nmap <Space> <Plug>(easymotion-overwin-f)
  let g:EasyMotion_smartcase = 1
"}}}
Plug 'scrooloose/nerdtree' "{{{
  noremap <leader>ft :NERDTreeToggle<CR>
  let g:NERDTreeAutoDeleteBuffer=1
  let g:NERDTreeShowHidden=1
"}}}

Plug 'Shougo/deoplete.nvim' "{{{
  let g:deoplete#enable_at_startup = 1
  set completeopt+=noinsert,noselect

  " Movement within 'ins-completion-menu'
  imap <expr><C-j>   pumvisible() ? "\<C-n>" : "\<C-j>"
  imap <expr><C-k>   pumvisible() ? "\<C-p>" : "\<C-k>"

  " " <CR>: If popup menu visible, expand snippet or close popup with selection,
  " "       Otherwise, check if within empty pair and use delimitMate.
  " imap <silent><expr><CR> pumvisible() ?
  "       \ (neosnippet#expandable() ? "\<Plug>(neosnippet_expand)" : "\<C-y>")
  "       \ : (delimitMate#WithinEmptyPair() ? "\<Plug>delimitMateCR" : "\<CR>")

  " Undo completion
  inoremap <expr><C-g> deoplete#mappings#undo_completion()

  " <Tab> completion:
  " 1. If popup menu is visible, select and insert next item
  " 2. Otherwise, if within a snippet, jump to next input
  " 3. Otherwise, if preceding chars are whitespace, insert tab char
  " 4. Otherwise, start manual autocomplete
  imap <silent><expr><Tab> pumvisible() ? "\<C-n>"
    \ : (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)"
    \ : (<SID>is_whitespace() ? "\<Tab>"
    \ : deoplete#mappings#manual_complete()))

  smap <silent><expr><Tab> pumvisible() ? "\<C-n>"
    \ : (neosnippet#jumpable() ? "\<Plug>(neosnippet_jump)"
    \ : (<SID>is_whitespace() ? "\<Tab>"
    \ : deoplete#mappings#manual_complete()))

  inoremap <expr><S-Tab>  pumvisible() ? "\<C-p>" : "\<C-h>"

  function! s:is_whitespace() "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~? '\s'
  endfunction "}}}
"}}}

Plug 't9md/vim-choosewin' "{{{
  nmap <leader>w <Plug>(choosewin)

  let g:choosewin_label = 'FGHJKLZXCVBNM'
  let g:choosewin_overlay_enable     = 1
  let g:choosewin_statusline_replace = 1
  let g:choosewin_tabline_replace    = 1
  let g:choosewin_label_padding      = 3
  let g:choosewin_blink_on_land      = 0

  let g:choosewin_color_label = {
    \ 'cterm': [ 236, 2 ], 'gui': [ '#555555', '#000000' ] }
  let g:choosewin_color_label_current = {
    \ 'cterm': [ 234, 220 ], 'gui': [ '#333333', '#000000' ] }
  let g:choosewin_color_other = {
    \ 'cterm': [ 235, 235 ], 'gui': [ '#333333' ] }
  let g:choosewin_color_overlay = {
    \ 'cterm': [ 2, 10 ], 'gui': [ '#88A2A4' ] }
  let g:choosewin_color_overlay_current = {
    \ 'cterm': [ 72, 64 ], 'gui': [ '#7BB292' ] }
"}}}

Plug 'https://github.com/env0der/delimitMate.git', { 'branch': 'optional-jump-over' } "{{{
 let g:delimitMate_expand_cr = 1
 let g:delimitMate_jump_over_in_insert_mode = 0
"}}}

Plug 'haya14busa/vim-asterisk' "{{{
	map *   <Plug>(asterisk-*)
	map #   <Plug>(asterisk-#)
	map g*  <Plug>(asterisk-g*)
	map g#  <Plug>(asterisk-g#)
	map z*  <Plug>(asterisk-z*)
	map gz* <Plug>(asterisk-gz*)
	map z#  <Plug>(asterisk-z#)
	map gz# <Plug>(asterisk-gz#)
  let g:asterisk#keeppos = 1
"}}}

Plug 'haya14busa/incsearch.vim' "{{{
  map /  <Plug>(incsearch-forward)
  map ?  <Plug>(incsearch-backward)
  map g/ <Plug>(incsearch-stay)
"}}}

Plug 'bkad/CamelCaseMotion' "{{{
	nmap <silent> e <Plug>CamelCaseMotion_e
	nmap <silent> w <Plug>CamelCaseMotion_w
	xmap <silent> w <Plug>CamelCaseMotion_w
	omap <silent> W <Plug>CamelCaseMotion_w
	nmap <silent> b <Plug>CamelCaseMotion_b
	xmap <silent> b <Plug>CamelCaseMotion_b
	omap <silent> B <Plug>CamelCaseMotion_b
"}}}

Plug 'kana/vim-textobj-user'
Plug 'rhysd/vim-textobj-anyblock'

" languages support
Plug 'guns/vim-clojure-highlight' "{{{
  au BufNewFile,BufRead *.edn set filetype=clojure
""}}}
Plug 'honza/dockerfile.vim'

Plug 'othree/yajs.vim', { 'for': 'javascript' }
" Automatically treat .es6 extension files as javascript
autocmd BufRead,BufNewFile *.es6 setfiletype javascript

Plug 'klen/python-mode', { 'for': 'python '}
let g:pymode_rope = 1

" Colors
Plug 'tomasr/molokai'
call plug#end()

colorscheme molokai
highlight LineNr ctermbg=none
set rnu
set number
set numberwidth=3
set noshowmode
set clipboard=unnamed
set tabstop=2
set shiftwidth=2
set expandtab smarttab

" case insensitive search
set ignorecase
set smartcase

" Switch between buffers without saving
set hidden

" Keep the cursor on the same column
set nostartofline
set regexpengine=1

inoremap jj <ESC>

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" reselect last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

nnoremap <leader>sc :nohl<cr>

" Enter adds new line beneath current line in normal mode
nmap <CR> :a<CR><CR>.<CR>
" C-j inserts new line at the current cursor position in normal mode
nmap <C-j> i<CR><ESC>

" shortcuts for windows {{{
  nnoremap <leader>w/ <C-w>v<C-w>l
  nnoremap <leader>w- <C-w>s
  nnoremap <m-h> <C-w>h
  nnoremap <m-j> <C-w>j
  nnoremap <m-k> <C-w>k
  nnoremap <m-l> <C-w>l
  tnoremap <m-h> <C-\><C-n><C-w>h
  tnoremap <m-l> <C-\><C-n><C-w>l
  tnoremap <m-j> <C-\><C-n><C-w>j
  tnoremap <m-k> <C-\><C-n><C-w>k
  au WinEnter term://* startinsert
"}}}

" kill current buffer
nnoremap <leader>bk :bd<CR>
" kill current tab
nnoremap <leader>tk :tabclose<CR>
" new tab
nnoremap <leader>tn :tabnew<CR>

" Zoom
function! s:zoom()
  if winnr('$') > 1
    tab split
  elseif len(filter(map(range(tabpagenr('$')), 'tabpagebuflist(v:val + 1)'),
                  \ 'index(v:val, '.bufnr('').') >= 0')) > 1
    tabclose
  endif
endfunction
nnoremap <silent> <leader>z :call <sid>zoom()<cr>
