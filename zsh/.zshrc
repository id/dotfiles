fpath=( /opt/homebrew/share/zsh/site-functions /usr/share/zsh/site-functions /usr/share/zsh/5.8/functions ${ASDF_DIR}/completions $fpath)

zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix 
zstyle :compinstall filename '.zshrc'

autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit
autoload -U select-word-style && select-word-style bash
autoload -U zmv

compdef -d ansible-vault

if [ -f $(brew --prefix)/etc/bash_completion.d/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM=verbose
    source $(brew --prefix)/etc/bash_completion.d/git-prompt.sh
    setopt PROMPT_SUBST ; PS1=$'[\e[32m%n\e[1m@\e[32m%m\e[31m$(__git_ps1 " (%s)") \e[33m%1~\e[0m]\n'
fi

alias e='emacsclient -t'
alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD | tr -d "\n"'

alias ls='ls -G'
alias ll='ls -alhG'
alias grep='grep --color=auto'

setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt APPEND_HISTORY            # append to history file
setopt HIST_NO_STORE             # Don't store history commands
. ~/.asdf/asdf.sh

