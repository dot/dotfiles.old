" インデントの設定
set autoindent

"tabの設定
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set listchars=tab:>-

set list
set nocompatible

syntax on
set number
"set wildmode=list:full
set shellslash
set hidden
set ruler
set wrapscan

" filetype
filetype on
filetype plugin on
filetype indent on

" キーマップの設定
"
" 表示行単位で行移動する
nnoremap j gj
nnoremap k gk

" カレントディレクトリのパスを挿入する
cnoremap <C-x> <C-r>=expand('%:p:h')<CR>/

" バックスペースでインデントや改行を削除できるようにする
set backspace=indent,eol,start

" 検索パスに追加
" let &path = &path.",~"

" ステータス行の表示
set statusline=%-50.50f\ %16(\ %m%r%{&fileencoding}\ %{&fileformat}%)%=%l\ /%5L


" スクロール時に前後に表示する行数
set scrolloff=3

" 日本語を扱う
set enc=utf-8
set fenc=utf-8
set fencs=utf-8,euc-jp,iso-2022-jp,cp932
set ffs=unix
set ambiwidth=single
" テキスト挿入中の自動折り返しを日本語に対応させる
set formatoptions+=mM
" " 日本語整形スクリプト(by. 西岡拓洋さん)用の設定
let format_allow_over_tw = 1    " ぶら下り可能幅

set fileformats=unix,dos,mac

colorscheme darkblue
" カーソル行をハイライト
set cursorline
" カレントウィンドウにのみ罫線を引く
augroup cch
    autocmd! cch
    autocmd WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
augroup END

:hi clear CursorLine
:hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

" Escの2回押しでハイライト消去
nmap <ESC><ESC> ;nohlsearch<CR><ESC>

" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//ge
" 保存時にtabをスペースに変換する
autocmd BufWritePre * :%s/\t/  /ge

"
imap {} {}<Left>
imap [] []<Left>
imap () ()<Left>
imap "" ""<Left>
imap '' ''<Left>
imap <> <><Left>

"ファイルタイプ別の設定
au FileType ruby,eruby set ts=2 sw=2 sts=2 expandtab
compiler ruby         " Enable compiler support for ruby
