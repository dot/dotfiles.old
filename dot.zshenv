#補完時に無視する接尾辞
fignore=('~')

# host settings
if [ -x /bin/hostname ]; then
    export HOST=`hostname`
fi;
export host=`echo $HOST | sed -e 's/\..*//'`

# coredumpファイルを吐かないようにする
#limit coredumpsize  0
limit maxproc 255  #40 is default
umask 022

export PAGER="lv -c"
export LV='-z -Ia -Ou8 -c'
export EDITOR='vim'

# locale
#unset LC_ALL
export LANG="ja_JP.UTF-8"
export LC_ALL="ja_JP.UTF-8"
#export LC_CTYPE=C

export TERM=xterm-256color

# for screen
export SCREENDIR=~/.screens/

####  path / PATH
path=( ~/bin \
    $HOME/.rbenv/bin \
    /usr/local/bin /usr/local/sbin /usr/local/mysql/bin \
    /usr/local/mongodb/bin \
    /usr/local/cuda/bin \
    /opt/local/bin /opt/local/sbin \
    /bin /sbin /usr/bin /usr/sbin \
)

fpath=(~/.functions ~/.functions/zsh-completions ${fpath})

export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case'
if type /usr/local/bin/lesspipe.sh &>/dev/null
then
    LESSOPEN="| /usr/local/bin/lesspipe.sh '%s'"
    LESSCLOSE="/usr/local/bin/lesspipe.sh '%s' '%s'"
fi

#list color
export LSCOLORS=gxfxcxdxbxegedabagacad
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
export ZLS_COLORS=$LS_COLORS

# local settings
if [ -e $HOME/.zshlocalenv ]; then
    source $HOME/.zshlocalenv
fi;

# login shell
#export SHELL=/usr/local/bin/zsh

#### time
REPORTTIME=8                    # CPUを8秒以上使った時は time を表示
TIMEFMT="\
    The name of this job.             :%J
    CPU seconds spent in user mode.   :%U
    CPU seconds spent in kernel mode. :%S
    Elapsed time in seconds.          :%E
    The  CPU percentage.              :%P"
