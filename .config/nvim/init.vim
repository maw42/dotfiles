" -----------------------------------------------------------------------------
" Plugins
" -----------------------------------------------------------------------------

	if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
		echo "Downloading junegunn/vim-plug to manage plugins..."
		silent !mkdir -p ~/.config/nvim/autoload/
		silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
		autocmd VimEnter * PlugInstall
	endif

	call plug#begin('~/.config/nvim/plugged')
	Plug 'tpope/vim-surround'
	Plug 'scrooloose/nerdtree'
	Plug 'junegunn/goyo.vim'
	Plug 'PotatoesMaster/i3-vim-syntax'
	" Plug 'jreybert/vimagit'
	Plug 'lukesmithxyz/vimling'
	Plug 'bling/vim-airline'
	Plug 'tpope/vim-commentary'
	Plug 'tpope/vim-obsession'
	Plug 'kovetskiy/sxhkd-vim'
	Plug 'will133/vim-dirdiff'
    Plug 'morhetz/gruvbox'
    Plug 'plasticboy/vim-markdown'
    " Plug 'junegunn/fzf', {'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    " ultisnips: snippet-functionality/engine. vim-snippets: contains snippets
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
    Plug 'nelstrom/vim-visual-star-search'
    Plug 'dhruvasagar/vim-zoom'
    " handles multi-file seach and replace, populates quickfix-list
    Plug 'mhinz/vim-grepper'
	Plug 'vimwiki/vimwiki'
	call plug#end()

" -----------------------------------------------------------------------------
" Color settings
" -----------------------------------------------------------------------------

    " colorscheme (after plugins, since e.g. gruvbox has special needs)
    " autocmd vimenter * ++nested colorscheme gruvbox
    " set background=dark
    colorscheme monokai
    set background=light
    " keep terminal transparency with colorscheme
    " autocmd vimenter * highlight Normal guibg=NONE ctermbg=NONE

" -----------------------------------------------------------------------------
" Basic settings
" -----------------------------------------------------------------------------

	"use space as mapleader and allow <SPACE> <SPACE>
	nnoremap <SPACE> <Nop>
	let mapleader ="\\"
	map  <SPACE> \

	syntax on
	filetype plugin on
	set nocompatible
	set bg=light
	set guioptions=a "should enable pasting in + and * register
	set clipboard+=unnamedplus
	set mouse=a "enable mouse support
	set tabstop=4 softtabstop=4
	set shiftwidth=4
	set expandtab
	set smartindent
    set ignorecase
	set smartcase
	set nohlsearch
	set incsearch
	set encoding=utf-8
    set t_Co=256
	set number relativenumber
	set wildmode=longest,list,full " Enable autocompletion:
	set splitbelow splitright " Splits open at the bottom and right, which is non-retarded, unlike vim defaults.

	set colorcolumn=80
    highlight ColorColumn ctermbg=0 guibg=lightgrey

	" Turns off highlighting on the bits of code that are changed, so the line that is changed is highlighted but the actual text that has changed stands out on the line and is readable.
	if &diff
	    highlight! link DiffText MatchParen
	endif

" -----------------------------------------------------------------------------
" Autocommands
" -----------------------------------------------------------------------------

	" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
	" Runs a script that cleans out tex build files whenever I close out of a .tex file.
	autocmd VimLeave *.tex !texclear %
	" Ensure files are read as what I want:
	autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex
	" Automatically deletes all trailing whitespace on save.
	autocmd BufWritePre * %s/\s\+$//e
	" When shortcut files are updated, renew bash and ranger configs with new material:
	autocmd BufWritePost files,directories !shortcuts
	" Run xrdb whenever Xdefaults or Xresources are updated.
	autocmd BufWritePost *Xresources,*Xdefaults !xrdb %
	" Update binds when sxhkdrc is updated.
	autocmd BufWritePost *sxhkdrc !pkill -USR1 sxhkd

" -----------------------------------------------------------------------------
" Plugin settings
" -----------------------------------------------------------------------------

	" --- DirDiff plugin uses enhanced mappings like <leader>dg <leader>dp <leader>dj <leader>dk
	let g:DirDiffEnableMappings = 1

	" --- Goyo plugin makes text more readable when writing prose:
	" Enable Goyo by default for mutt writting
	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=light
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<CR>

	map <leader>f :Goyo \| set bg=light \| set linebreak<CR>

	" --- Nerd tree
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
	map <leader>n :NERDTreeToggle<CR>

	" --- vimling: for writing special characters, IPA, and prose
	nmap <leader>d :call ToggleDeadKeys()<CR>
	imap <leader>d <esc>:call ToggleDeadKeys()<CR>a
	nmap <leader>i :call ToggleIPA()<CR>
	imap <leader>i <esc>:call ToggleIPA()<CR>a
	nmap <leader>q :call ToggleProse()<CR>

	" --- set up vimwiki
	let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
	let g:vimwiki_list = [{'path': '/mnt/extData/Daten/Getting Things Done/Referenzen/Text_Stuff/vimwiki', 'syntax': 'markdown', 'ext': '.md'}, {'path': '~/Documents/vimwiki', 'syntax': 'markdown', 'ext': '.md'}, {'path': '/mnt/extData/Daten/Getting Things Done/Referenzen/IT/Linux/linuxwiki', 'syntax': 'markdown', 'ext': '.md'} ]
    let g:vimwiki_global_ext = 0 " use markdown filetype outside vimwiki paths
    " Allow vimwiki to use markdown folding
    let g:vimwiki_folding='custom'
    let g:vimwiki_markdown_link_ext = 1 "append .md to new markdown wikis
    autocmd FileType vimwiki setlocal syntax=markdown
    autocmd FileType vimwiki setlocal foldenable
    " Disable vimwiki-keybindings and set them manually (refer to 'h: vimwiki-local-mappings')
    let g:vimwiki_key_mappings = { 'all_maps':0, }
    " (local binding) enter / create link
    autocmd FileType vimwiki nmap <CR> <Plug>VimwikiFollowLink
    autocmd FileType vimwiki xmap <CR> <Plug>VimwikiFollowLink
    " (global binding) open vimwiki index
    nnoremap <leader>ww :VimwikiIndex<CR>
    " (global binding) open vimwiki index as new tab
    nnoremap <leader>wt :VimwikiTabIndex<CR>


    " --- Ultisnips config
    " Trigger configuration. You need to change this to something other than <tab> if you use one of the following:
    " - https://github.com/Valloric/YouCompleteMe
    " - https://github.com/nvim-lua/completion-nvim
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<c-b>"
    let g:UltiSnipsJumpBackwardTrigger="<c-z>"

    " If you want :UltiSnipsEdit to split your window.
    let g:UltiSnipsEditSplit="vertical"
    " directory for own snippets
    " let g:UltiSnipsSnippetsDir="~/.config/nvim/mysnippets"
    " path to look for snippets
    let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]

    " --- vim-zoom config
        " binding for zooming in and out
    nmap <C-m> <Plug>(zoom-toggle)

    " --- fzf
    let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'
    " Launch fzf with CTRL+P.
    nnoremap <silent> <C-p> :FZF -m<CR>
    " Map a few common things to do with FZF.
    nnoremap <silent> <Leader><Enter> :Buffers<CR>
    nnoremap <silent> <Leader>l :Lines<CR>
    " other fzf-stuff
    nnoremap <C-p> :GFiles<CR>
    nnoremap <leader>pf :Files<CR>
    nnoremap <leader>ps :Rg<SPACE>

    " --- Ripgrep Rg
    " Allow passing optional flags into the Rg command.
    "   Example: :Rg myterm -g '*.md'
    command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \ "rg --column --line-number --no-heading --color=always --smart-case " .
      \ <q-args>, 1, fzf#vim#with_preview(), <bang>0)

    " --- mhinz/vim-grepper
    let g:grepper={}
    let g:grepper.tools=["rg"]

    xmap gr <plug>(GrepperOperator)

    " After searching for text, press this mapping to do a project wide find and
    " replace. It's similar to <leader>r except this one applies to all matches
    " across all files instead of just the current file.
    nnoremap <Leader>R
      \ :let @s='\<'.expand('<cword>').'\>'<CR>
      \ :Grepper -cword -noprompt<CR>
      \ :cfdo %s/<C-r>s//g \| update
      \<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>

    " The same as above except it works with a visual selection.
    xmap <Leader>R
        \ "sy
        \ gvgr
        \ :cfdo %s/<C-r>s//g \| update
         \<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>

" -----------------------------------------------------------------------------
" Remaps
" -----------------------------------------------------------------------------

" Load command shortcuts generated from bm-dirs and bm-files via shortcuts script.
" Here leader is ";".
" So ":vs ;cfz" will expand into ":vs /home/<user>/.config/zsh/.zshrc"
" if typed fast without the timeout.
    " source ~/.config/nvim/shortcuts.vim

    " -------------------------------------------------------------------------
    " General Remaps
    " -------------------------------------------------------------------------

	" change operation does not overwrite unnamed register
	nnoremap c "_c
	" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>
	" Shortcutting split navigation, saving a keypress:
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l

	" Check file in shellcheck: (checks shell scripts for errors)
	map <leader>s :!clear && shellcheck %<CR>

	" Open my bibliography file in split
	map <leader>b :vsp<space>$BIB<CR>
	map <leader>r :vsp<space>$REFER<CR>

	" insert date on F5
	:nnoremap <F5> "=strftime("%Y-%m-%d")<CR>P
	:inoremap <F5> <C-R>=strftime("%Y-%m-%d")<CR>
    " insert Vimwiki-date on F6
	:nnoremap <F6> "=strftime("%Y%m%d%H%M%S")<CR>P
	:inoremap <F6> <C-R>=strftime("%Y%m%d%H%M%S")<CR>
	" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

    " Toggle visually showing all whitespace characters.
    noremap <F7> :set list!<CR>
    inoremap <F7> <C-o>:set list!<CR>
    cnoremap <F7> <C-c>:set list!<CR>

	" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compiler <c-r>%<CR>

	" Open corresponding .pdf/.html or preview
	map <leader>p :!opout <c-r>%<CR><CR>
	" Save file as sudo on files that require root permission
	cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!
	"" Navigating with guides
	" inoremap <space><space> <Esc>/<<Enter>"_c4l
	vnoremap <leader><space> <Esc>/<++><Enter>"_c4l
	map <leader><space> <Esc>/<++><Enter>"_c4l

    nnoremap <leader>u :UndotreeShow<CR>
    nnoremap <leader>pe :wincmd v<bar> :wincmd h<bar> :Ex <bar> :vertical resize 30<CR>

    " Press * to search for the term under the cursor or a visual selection and
    " then press a key below to replace all instances of it in the current file.
    nnoremap <Leader>r :%s///g<Left><Left>
    nnoremap <Leader>rc :%s///gc<Left><Left><Left>

    " The same as above but instead of acting on the whole file it will be
    " restricted to the previously visually selected range. You can do that by
    " pressing *, visually selecting the range you want it to apply to and then
    " press a key below to replace all instances of it in the current selection.
    xnoremap <Leader>r :s///g<Left><Left>
    xnoremap <Leader>rc :s///gc<Left><Left><Left>

    " Type a replacement term and press . to repeat the replacement again. Useful
    " for replacing a few instances of the term (comparable to multiple cursors).
    nnoremap <silent> s* :let @/='\<'.expand('<cword>').'\>'<CR>cgn
    xnoremap <silent> s* "sy:let @/=@s<CR>cgn



    " -------------------------------------------------------------------------
    " Filetype-specific remaps
    " -------------------------------------------------------------------------

        "---.VIMRC---"
        autocmd! bufwritepost init.vim,.vimrc source % | echom "reloaded vimrc"

        ""---LATEX---"
        "" Word count:
        "autocmd FileType tex map <leader>w :w !detex \| wc -w<CR>
        "" Code snippets
        "autocmd FileType tex inoremap ,fr \begin{frame}<Enter>\frametitle{}<Enter><Enter><++><Enter><Enter>\end{frame}<Enter><Enter><++><Esc>6kf}i
        "autocmd FileType tex inoremap ,fi \begin{fitch}<Enter><Enter>\end{fitch}<Enter><Enter><++><Esc>3kA
        "autocmd FileType tex inoremap ,exe \begin{exe}<Enter>\ex<Space><Enter>\end{exe}<Enter><Enter><++><Esc>3kA
        "autocmd FileType tex inoremap ,em \emph{}<++><Esc>T{i
        "autocmd FileType tex inoremap ,bf \textbf{}<++><Esc>T{i
        "autocmd FileType tex vnoremap , <ESC>`<i\{<ESC>`>2la}<ESC>?\\{<Enter>a
        "autocmd FileType tex inoremap ,it \textit{}<++><Esc>T{i
        "autocmd FileType tex inoremap ,ct \textcite{}<++><Esc>T{i
        "autocmd FileType tex inoremap ,cp \parencite{}<++><Esc>T{i
        "autocmd FileType tex inoremap ,glos {\gll<Space><++><Space>\\<Enter><++><Space>\\<Enter>\trans{``<++>''}}<Esc>2k2bcw
        "autocmd FileType tex inoremap ,x \begin{xlist}<Enter>\ex<Space><Enter>\end{xlist}<Esc>kA<Space>
        "autocmd FileType tex inoremap ,ol \begin{enumerate}<Enter><Enter>\end{enumerate}<Enter><Enter><++><Esc>3kA\item<Space>
        "autocmd FileType tex inoremap ,ul \begin{itemize}<Enter><Enter>\end{itemize}<Enter><Enter><++><Esc>3kA\item<Space>
        "autocmd FileType tex inoremap ,li <Enter>\item<Space>
        "autocmd FileType tex inoremap ,ref \ref{}<Space><++><Esc>T{i
        "autocmd FileType tex inoremap ,tab \begin{tabular}<Enter><++><Enter>\end{tabular}<Enter><Enter><++><Esc>4kA{}<Esc>i
        "autocmd FileType tex inoremap ,ot \begin{tableau}<Enter>\inp{<++>}<Tab>\const{<++>}<Tab><++><Enter><++><Enter>\end{tableau}<Enter><Enter><++><Esc>5kA{}<Esc>i
        "autocmd FileType tex inoremap ,can \cand{}<Tab><++><Esc>T{i
        "autocmd FileType tex inoremap ,con \const{}<Tab><++><Esc>T{i
        "autocmd FileType tex inoremap ,v \vio{}<Tab><++><Esc>T{i
        "autocmd FileType tex inoremap ,a \href{}{<++>}<Space><++><Esc>2T{i
        "autocmd FileType tex inoremap ,sc \textsc{}<Space><++><Esc>T{i
        "autocmd FileType tex inoremap ,chap \chapter{}<Enter><Enter><++><Esc>2kf}i
        "autocmd FileType tex inoremap ,sec \section{}<Enter><Enter><++><Esc>2kf}i
        "autocmd FileType tex inoremap ,ssec \subsection{}<Enter><Enter><++><Esc>2kf}i
        "autocmd FileType tex inoremap ,sssec \subsubsection{}<Enter><Enter><++><Esc>2kf}i
        "autocmd FileType tex inoremap ,st <Esc>F{i*<Esc>f}i
        "autocmd FileType tex inoremap ,beg \begin{DELRN}<Enter><++><Enter>\end{DELRN}<Enter><Enter><++><Esc>4k0fR:MultipleCursorsFind<Space>DELRN<Enter>c
        "autocmd FileType tex inoremap ,up <Esc>/usepackage<Enter>o\usepackage{}<Esc>i
        "autocmd FileType tex nnoremap ,up /usepackage<Enter>o\usepackage{}<Esc>i
        "autocmd FileType tex inoremap ,tt \texttt{}<Space><++><Esc>T{i
        "autocmd FileType tex inoremap ,bt {\blindtext}
        "autocmd FileType tex inoremap ,nu $\varnothing$
        "autocmd FileType tex inoremap ,col \begin{columns}[T]<Enter>\begin{column}{.5\textwidth}<Enter><Enter>\end{column}<Enter>\begin{column}{.5\textwidth}<Enter><++><Enter>\end{column}<Enter>\end{columns}<Esc>5kA
        "autocmd FileType tex inoremap ,rn (\ref{})<++><Esc>F}i

        ""---HTML---"
		"autocmd FileType html inoremap ,b <b></b><Space><++><Esc>FbT>i
		"autocmd FileType html inoremap ,it <em></em><Space><++><Esc>FeT>i
		"autocmd FileType html inoremap ,1 <h1></h1><Enter><Enter><++><Esc>2kf<i
		"autocmd FileType html inoremap ,2 <h2></h2><Enter><Enter><++><Esc>2kf<i
		"autocmd FileType html inoremap ,3 <h3></h3><Enter><Enter><++><Esc>2kf<i
		"autocmd FileType html inoremap ,p <p></p><Enter><Enter><++><Esc>02kf>a
		"autocmd FileType html inoremap ,a <a<Space>href=""><++></a><Space><++><Esc>14hi
		"autocmd FileType html inoremap ,e <a<Space>target="_blank"<Space>href=""><++></a><Space><++><Esc>14hi
		"autocmd FileType html inoremap ,ul <ul><Enter><li></li><Enter></ul><Enter><Enter><++><Esc>03kf<i
		"autocmd FileType html inoremap ,li <Esc>o<li></li><Esc>F>a
		"autocmd FileType html inoremap ,ol <ol><Enter><li></li><Enter></ol><Enter><Enter><++><Esc>03kf<i
		"autocmd FileType html inoremap ,im <img src="" alt="<++>"><++><esc>Fcf"a
		"autocmd FileType html inoremap ,td <td></td><++><Esc>Fdcit
		"autocmd FileType html inoremap ,tr <tr></tr><Enter><++><Esc>kf<i
		"autocmd FileType html inoremap ,th <th></th><++><Esc>Fhcit
		"autocmd FileType html inoremap ,tab <table><Enter></table><Esc>O
		"autocmd FileType html inoremap ,gr <font color="green"></font><Esc>F>a
		"autocmd FileType html inoremap ,rd <font color="red"></font><Esc>F>a
		"autocmd FileType html inoremap ,yl <font color="yellow"></font><Esc>F>a
		"autocmd FileType html inoremap ,dt <dt></dt><Enter><dd><++></dd><Enter><++><esc>2kcit
		"autocmd FileType html inoremap ,dl <dl><Enter><Enter></dl><enter><enter><++><esc>3kcc
		"autocmd FileType html inoremap &<space> &amp;<space>
		"autocmd FileType html inoremap á &aacute;
		"autocmd FileType html inoremap é &eacute;
		"autocmd FileType html inoremap í &iacute;
		"autocmd FileType html inoremap ó &oacute;
		"autocmd FileType html inoremap ú &uacute;
		"autocmd FileType html inoremap ä &auml;
		"autocmd FileType html inoremap ë &euml;
		"autocmd FileType html inoremap ï &iuml;
		"autocmd FileType html inoremap ö &ouml;
		"autocmd FileType html inoremap ü &uuml;
		"autocmd FileType html inoremap ã &atilde;
		"autocmd FileType html inoremap ẽ &etilde;
		"autocmd FileType html inoremap ĩ &itilde;
		"autocmd FileType html inoremap õ &otilde;
		"autocmd FileType html inoremap ũ &utilde;
		"autocmd FileType html inoremap ñ &ntilde;
		"autocmd FileType html inoremap à &agrave;
		"autocmd FileType html inoremap è &egrave;
		"autocmd FileType html inoremap ì &igrave;
		"autocmd FileType html inoremap ò &ograve;
		"autocmd FileType html inoremap ù &ugrave;

        "---.bib---"
		autocmd FileType bib inoremap ,a @article{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>journal<Space>=<Space>{<++>},<Enter>volume<Space>=<Space>{<++>},<Enter>pages<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i
		autocmd FileType bib inoremap ,b @book{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>6kA,<Esc>i
		autocmd FileType bib inoremap ,c @incollection{<Enter>author<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>booktitle<Space>=<Space>{<++>},<Enter>editor<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i

        ""---MARKDOWN---"
		"autocmd Filetype markdown,rmd map <leader>w yiWi[<esc>Ea](<esc>pa)
		"autocmd Filetype markdown,rmd inoremap ,n ---<Enter><Enter>
		"autocmd Filetype markdown,rmd inoremap ,b ****<++><Esc>F*hi
		"autocmd Filetype markdown,rmd inoremap ,s ~~~~<++><Esc>F~hi
		"autocmd Filetype markdown,rmd inoremap ,e **<++><Esc>F*i
		"autocmd Filetype markdown,rmd inoremap ,h ====<Space><++><Esc>F=hi
		"autocmd Filetype markdown,rmd inoremap ,i ![](<++>)<++><Esc>F[a
		"autocmd Filetype markdown,rmd inoremap ,a [](<++>)<++><Esc>F[a
		"autocmd Filetype markdown,rmd inoremap ,1 #<Space><Enter><++><Esc>kA
		"autocmd Filetype markdown,rmd inoremap ,2 ##<Space><Enter><++><Esc>kA
		"autocmd Filetype markdown,rmd inoremap ,3 ###<Space><Enter><++><Esc>kA
		"autocmd Filetype markdown,rmd inoremap ,l --------<Enter>
		"autocmd Filetype rmd inoremap ,r ```{r}<CR>```<CR><CR><esc>2kO
		"autocmd Filetype rmd inoremap ,p ```{python}<CR>```<CR><CR><esc>2kO
		"autocmd Filetype rmd inoremap ,c ```<cr>```<cr><cr><esc>2kO

        ""---.xml---"
		"autocmd FileType xml inoremap ,e <item><Enter><title><++></title><Enter><guid<space>isPermaLink="false"><++></guid><Enter><pubDate><Esc>:put<Space>=strftime('%a, %d %b %Y %H:%M:%S %z')<Enter>kJA</pubDate><Enter><link><++></link><Enter><description><![CDATA[<++>]]></description><Enter></item><Esc>?<title><enter>cit
		"autocmd FileType xml inoremap ,a <a href="<++>"><++></a><++><Esc>F"ci"

