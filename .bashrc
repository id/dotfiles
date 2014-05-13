# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

export PS1="\[\e]0;\w\a\]\n\[\e[32m\]\l:\u@\h \[\e[33m\]\w\[\e[0m\]\n"

export PATH=$PATH:$HOME/bin

alias e='emacsclient -t'
alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD'
alias dirsize='du -h --max-depth=1'
alias gs='git status --short -uno'

export EDITOR='emacsclient -t'
