# Source global definitions
[ -f /etc/bashrc ] && . /etc/bashrc
[ -f /etc/bash_completion ] && . /etc/bash_completion

GIT_PROMPT=~/.git-prompt.sh
if [ -f $GIT_PROMPT ]; then
    . $GIT_PROMPT
    export PS1='\e[0m\n[\D{%F %T}] \e[32m\u\e[0m\e[1m@\e[32m\h\e[0m\e[31m\]$(__git_ps1 " [%s]") \e[33m\w\e[0m\n'
else
    export PS1='\e[0m\n[\D{%F %T}] \e[32m\u\e[0m\e[1m@\e[32m\h\e[0m \e[33m\w\e[0m\n'
fi

export PATH=$PATH:$HOME/bin
export HISTCONTROL=ignoredups
export HISTSIZE=10000
export HISTTIMEFORMAT="[%F %T] "
export LC_CTYPE="en_US.UTF-8"
export EDITOR='emacsclient -t'
export GPG_TTY=$(tty)

if [ $(uname) == Darwin ]; then
    export LSCOLORS="Hxfxcxdxbxegedabagacad"
    alias ls='ls -G'
    alias ll='ls -alhG'
else
    alias ls='ls --color=auto'
    alias ll='ls -al --color=auto'
fi

alias e='emacsclient -t'
alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD'
alias dirsize='du -h --max-depth=1'
alias gs='git status --short -uno'
alias grep='grep --color=auto'
alias fuck='sudo $(history -p \!\!)'

port_pids() {
  sudo netstat -ltp | grep $1 | sort -u
}

kill_port_pids() {
  sudo netstat -ltp | grep $1 | awk '{print substr($7,1,index($7, "/") - 1)}' | \
      sort -u | xargs sudo kill
}

if [ `uname` == Darwin ]; then
    export LSCOLORS="Hxfxcxdxbxegedabagacad"
    alias ls='ls -G'
    alias ll='ls -alhG'
else
    alias ls='ls --color=auto'
    alias ll='ls -al --color=auto'
fi

calc() {
    local result=""
    result="$(printf "$*\n" | bc --mathlib | tr -d '\\\n')"
    if [ "*.*" = "$result" ]; then
        # improve the output for decimal numbers
        printf "$result" |
        sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
            -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
            -e 's/0*$//;s/\.$//'   # remove trailing zeros
        else
            printf "$result"
    fi
    printf "\n"
}

# host custom config
[ -f ~/.bashrc.custom ] && . ~/.bashrc.custom
