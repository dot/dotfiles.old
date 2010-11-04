source ~/.aliases

# about history
HISTFILE="$HOME/.zhistory"
HISTSIZE=100000
SAVEHIST=100000

setopt extended_history         # コマンドの開始時刻と経過時間を登録
function history-all { history -E 1 }


autoload -U compinit; compinit -u
compinit -d /tmp/$USER.zcompdump
_cache_hosts=(`perl -ne  'if (/^([a-zA-Z0-9.-]+)/) { print "$1\n";}' ~/.ssh/known_hosts`)

# zstyles
zstyle ':completion:*' use-compctl false # compctl形式を使用しない
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'
zstyle ':completion:*' list-colors $LS_COLORS


# 単語単位でのバックスペース
export WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

autoload -U colors; colors      # ${fg[red]}形式のカラー書式を有効化

hosts=( localhost `hostname` )
cdpath=( ~ )                    # cd のサーチパス

#### option, limit, bindkey
setopt auto_list
setopt auto_menu
setopt auto_param_keys
#setopt extended_history
setopt hist_expand
setopt hist_ignore_dups         # 直前のコマンドと同一ならば登録しない
setopt hist_ignore_all_dups     # 登録済コマンド行は古い方を削除
setopt hist_reduce_blanks       # 余分な空白は詰めて登録(空白数違い登録を防ぐ)
setopt hist_no_store            # historyコマンドは登録しない
setopt hist_ignore_space        # コマンド行先頭が空白の時登録しない(直後ならば呼べる)
#setopt hist_verify
setopt append_history           # zsh を終了させた順にファイルに記録(デフォルト)
setopt inc_append_history       # 同上、ただしコマンドを入力した時点で記録
setopt share_history            # ヒストリの共有。    
setopt list_packed              # 補完候補リストを詰めて表示
setopt print_eight_bit          # 補完候補リストの日本語を適正表示
setopt no_clobber               # 上書きリダイレクトの禁止
setopt no_unset                 # 未定義変数の使用の禁止
setopt no_hup                   # logout時にバックグラウンドジョブを kill しない
setopt no_beep                  # コマンド入力エラーでBEEPを鳴らさない
setopt extended_glob            # 拡張グロブ
setopt numeric_glob_sort        # 数字を数値と解釈して昇順ソートで出力
setopt auto_cd                  # 第1引数がディレクトリだと cd を補完
setopt auto_pushd
setopt correct                  # スペルミス補完
setopt no_checkjobs             # exit 時にバックグラウンドジョブを確認しない
setopt ignore_eof               # C-dでlogoutしない(C-dを補完で使う人用)
setopt pushd_to_home            # 引数なしpushdで$HOMEに戻る(直前dirへは cd - で)
setopt pushd_ignore_dups        # ディレクトリスタックに重複する物は古い方を削除
#setopt pushd_silent            # pushd, popd の度にディレクトリスタックの中身を表示しない
setopt interactive_comments     # コマンド入力中のコメントを認める
#setopt rm_star_silent          # rm * で本当に良いか聞かずに実行
#setopt chase_links             # リンク先のパスに変換してから実行。
setopt auto_pushd               # cdで自動的にpushする
setopt multios                  # 複数のリダイレクトを許可する
setopt complete_in_word
setopt print_eight_bit          #
setopt list_packed              # できるだけ節約してリスト表示
setopt list_types
setopt long_list_jobs
setopt mark_dirs                # ディレクトリにマッチしたら/を付ける
unsetopt auto_remove_slash      # 最後がディレクトリ名で終わっている場合末尾の / を自動的に取り除かない
setopt auto_param_slash         # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt brace_ccl
setopt magic_equal_subst
setopt print_eightbit
setopt short_loops
setopt transient_rprompt

stty    erase   '^H'
stty    intr    '^C'
stty    susp    '^Z'

#### bindkey
bindkey -e    # emacs like keybind 
bindkey '^I' complete-word
bindkey '^|' expand-or-complete-prefix
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward
bindkey '^X^F' forward-word
bindkey '^X^B' backward-word

autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey '^X^Z' predict-on
bindkey '^Z' predict-off
zstyle ':predict' verbose true
autoload run-help

autoload zmv
alias zmv='noglob zmv'

# replace
autoload -U replace-string
zle -N replace-string


# dabbrev
HARDCOPYFILE=$HOME/tmp/screen-hardcopy
touch $HARDCOPYFILE

dabbrev-complete () {
    local reply lines=80 # 80行分
    screen -X eval "hardcopy -h $HARDCOPYFILE"
    reply=($(sed '/^$/d' $HARDCOPYFILE | sed '$ d' | tail -$lines))
    compadd - "${reply[@]%[*/=@|]}"
}

zle -C dabbrev-complete menu-complete dabbrev-complete
bindkey '^[/' dabbrev-complete
bindkey '^[/^_' reverse-menu-complete

# insert previous command last word
autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match \
  '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word

# quote previous word in single or double quote
autoload -U modify-current-argument
_quote-previous-word-in-single() {
    modify-current-argument '${(qq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-single
bindkey '^[s' _quote-previous-word-in-single

_quote-previous-word-in-double() {
    modify-current-argument '${(qqq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-double
bindkey '^[d' _quote-previous-word-in-double


# up command
# http://gist.github.com/607290
# http://d.hatena.ne.jp/hitode909/20101002/1285994430
function up()
{
    to=$(perl -le '$p=$ENV{PWD}."/";$d="/".$ARGV[0]."/";$r=rindex($p,$d);$r>=0 && print substr($p, 0, $r+length($d))' $1)
    if [ "$to" = "" ]; then
        echo "no such file or directory: $1" 1>&2
        return 1
    fi
    cd $to
}

############################################################
## プロンプト設定
unsetopt promptcr               # 改行のない出力をプロンプトで上書きするのを防ぐ
setopt prompt_subst             # ESCエスケープを有効にする
autoload -Uz vcs_info

zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b] (%a)'

precmd () {
    psvar=()
    vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

#PROMPT=$'%{$fg[green]%}%n@%M %{$fg[cyan]%}%~ %1(v|%F{green}%1v%f|)\n%{$fg[yellow]%}%#%{$reset_color%} '
PROMPT=$'%{$fg[yellow]%}%n%{$fg[red]%}@$fg[green]%}%m %{$fg[cyan]%}%~ %1(v|%F{green}%1v%f|)\n%{$fg[green]%}%#%{$reset_color%} '

autoload zmv
alias zmv='noglob zmv'


# rake command completion
_rake_does_task_list_need_generating () {
  if [ ! -f .rake_tasks ]; then return 0;
  else
    accurate=$(stat -f%m .rake_tasks)
    changed=$(stat -f%m Rakefile)
    return $(expr $accurate '>=' $changed)
  fi
}

_rake () {
  if [ -f Rakefile ]; then
    if _rake_does_task_list_need_generating; then
      echo "\nGenerating .rake_tasks..." > /dev/stderr
      rake --silent --tasks | cut -d " " -f 2 > .rake_tasks
    fi
    compadd $(<.rake_tasks)
  fi
}

compdef _rake rake

function cdb() {
  cd `ruby -e "require 'rubygems';gem 'bundler';require 'bundler';Bundler.load.specs.each{|s| puts s.full_gem_path if s.name == '${1}'}"`
}