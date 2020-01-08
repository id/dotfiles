if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
    source "$(brew --prefix)/opt/zsh-git-prompt/zshrc.sh"
fi

autoload -Uz compinit && compinit

if [ -f $(brew --prefix)/etc/bash_completion.d/git-prompt.sh ]; then
    source $(brew --prefix)/etc/bash_completion.d/git-prompt.sh
    setopt PROMPT_SUBST ; PS1=$'[\e[32m%n\e[1m@\e[32m%m\e[31m$(__git_ps1 " (%s)") \e[33m%c\e[0m] '
fi

alias e='emacsclient -t'
alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD'

export LSCOLORS="Hxfxcxdxbxegedabagacad"
alias ls='ls -G'
alias ll='ls -alhG'
alias grep='grep --color=auto'

export EDITOR='emacsclient -t'
