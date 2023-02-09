fpath=( /opt/homebrew/share/zsh/site-functions /usr/share/zsh/site-functions /usr/share/zsh/*/functions ${ASDF_DIR}/completions $fpath)

zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
[ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ] && zstyle ':completion:*:*:git:*' script /opt/homebrew/etc/bash_completion.d/git-completion.bash
zstyle :compinstall filename '.zshrc'

autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit
autoload -U select-word-style && select-word-style bash
autoload -U zmv

compdef -d ansible-vault

if [ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM=verbose
    source /opt/homebrew/etc/bash_completion.d/git-prompt.sh
    setopt PROMPT_SUBST ; PS1=$'[\e[32m%n\e[1m@\e[32m%m\e[31m$(__git_ps1 " (%s)") \e[33m%1~\e[0m]\n'
fi

alias e='emacsclient -t'
alias ls='ls -G'
alias ll='ls -alhG'
alias grep='grep --color=always'
alias grepn='grep --color=always -n'
alias erlgrep="find . -name '*.erl' | xargs grep --color=always -n"
alias alias br='git rev-parse --symbolic-full-name --abbrev-ref HEAD | tr -d "\n"'
alias gfa='git fetch --all --prune'
alias gfu='git fetch upstream --prune'
alias gru='git rebase upstream/master'
alias gpom='git push origin master:master'
alias delete-merged="git branch --merged | grep -Ev 'master|main|release' | grep -v '*' | xargs git branch --delete"

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

function kerl-activate() {
    source $HOME/.kerl/installations/$1/activate
}

function new-branch() {
    git checkout -b $1 && git push -u origin $1:$1
}

function gg() {
    find . -name "*.${1}" | xargs grep --color=always "${2}"
}


aws-unset() {
    unset AWS_PROFILE
    unset AWS_ACCESS_KEY_ID
    unset AWS_SECRET_ACCESS_KEY
    unset AWS_SESSION_TOKEN
}

function brew-unset() {
    unset HOMEBREW_NO_AUTO_UPDATE
    unset HOMEBREW_NO_INSTALL_CLEANUP
    unset HOMEBREW_NO_INSTALL_UPGRADE
    unset HOMEBREW_NO_ANALYTICS
    unset HOMEBREW_AUTO_UPDATE_SECS
    unset HOMEBREW_CLEANUP_MAX_AGE_DAYS
    unset HOMEBREW_INSTALL_FROM_API
    unset HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK
    unset HOMEBREW_NO_GITHUB_API
}

[ -f '/opt/homebrew/opt/asdf/libexec/asdf.sh' ] && source '/opt/homebrew/opt/asdf/libexec/asdf.sh'
[ -f '/opt/gcloud/google-cloud-sdk/path.zsh.inc' ] && source '/opt/gcloud/google-cloud-sdk/path.zsh.inc'
[ -f '/opt/gcloud/google-cloud-sdk/completion.zsh.inc' ] && source '/opt/gcloud/google-cloud-sdk/completion.zsh.inc'
