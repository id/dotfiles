autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit
autoload -Uz promptinit && promptinit

fpath=( ~/.zsh_functions "${fpath[@]}" )
autoload -U $fpath[1]/*(.:t)

if [ -f $(brew --prefix)/etc/bash_completion.d/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM=verbose
    source $(brew --prefix)/etc/bash_completion.d/git-prompt.sh
    setopt PROMPT_SUBST ; PS1=$'[\e[32m%n\e[1m@\e[32m%m\e[31m$(__git_ps1 " (%s)") \e[33m%1~\e[0m]\n'
fi

alias e='emacsclient -t'
alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD'

alias ls='ls -G'
alias ll='ls -alhG'
alias grep='grep --color=auto'

export EDITOR='emacsclient -t'
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
export WORDCHARS=${WORDCHARS/\/}
