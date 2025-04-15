if type /opt/homebrew/bin/brew &>/dev/null
then
  eval "$(/opt/homebrew/bin/brew shellenv zsh)"
fi

autoload -Uz compinit && compinit

zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' menu yes select
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:processes' command 'ps aux'
zstyle ':completion:*:processes' sort false
zstyle ':completion:*:processes-names' command 'ps xho command'

compdef -d ansible-vault

unalias run-help 2>/dev/null
alias help=run-help
autoload -Uz run-help
autoload -Uz run-help-git

autoload -U select-word-style && select-word-style bash
autoload -U zmv
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

setopt PROMPT_SUBST ;
if [ -f $HOMEBREW_PREFIX/etc/bash_completion.d/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=yes # unstaged (*) and staged (+) changes will be shown next to the branch name
    export GIT_PS1_SHOWSTASHSTATE=yes # '$' will be shown next to the branch name
    export GIT_PS1_SHOWUNTRACKEDFILES=yes # '%' will be shown next to the branch name
    export GIT_PS1_SHOWUPSTREAM=(git verbose) # show number of commits ahead/behind (+/-) upstream and upstream abbrev name
    export GIT_PS1_SHOWCONFLICTSTATE=yes # The prompt will include "|CONFLICT"
    export GIT_PS1_SHOWCOLORHINTS=yes
    export GIT_PS1_DESCRIBE_STYLE='describe'
    source $HOMEBREW_PREFIX/etc/bash_completion.d/git-prompt.sh
    PROMPT=$'%F{8}%*%f %F{yellow}%~%f%F{green}$(__git_ps1 " (%s)")%f\n'
else
    PROMPT=$'%F{8}%*%f %F{yellow}%~%f\n'
fi

alias e='emacsclient -t'
alias ls='ls -G'
alias ll='ls -alhGF'
alias g='grep --color=never'
alias grep='grep --color=auto'
alias grepn='grep --color=auto -n'
alias erlgrep="find . -name '*.erl' | xargs grep --color=auto -n"
alias br='git branch --show-current 2> /dev/null'
alias grum='git rebase upstream/master'
alias gpom='git push origin master:master'
alias delete-merged="git branch --merged | /usr/bin/grep -Ev 'master|main' | /usr/bin/grep -v '*' | xargs git branch --delete" #
alias tf=terraform
alias ssh0='ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null'
alias rsync0="rsync -e 'ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null'"
alias myip='curl -sS ifconfig.me'
# alias myip2='curl -sS ipinfo.io/ip'
# alias myip3='curl -sS https://am.i.mullvad.net/'
# alias myip4='dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com | tr -d \"'
# alias myip5='curl wasab.is'
# alias myip6='curl -sL ip.guide | jq -r .ip'
alias myipinfo='curl -sL ip.guide'
# alias myipinfo2='curl -sL ipinfo.io'
# alias myipinfo3='curl -sS https://am.i.mullvad.net/json | jq .'
alias myasn='whois -h bgp.tools " -v $(curl -s ifconfig.me)"'
# alias myasn2='echo $(curl -sS ifconfig.me) | nc bgp.tools 43'
alias asn='whois -h bgp.tools " -v $*"'

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

ulimit -n 122880

function kerl-activate() {
    source $HOME/.kerl/installations/$1/activate
}

function new-branch() {
    git checkout -b $1 && git push -u origin $1:$1
}

function gg() {
    find . -name "*.${1}" | xargs grep --color=always "${2}"
}

function drvw () {
    cat <<EOF > /tmp/gitconfig
[safe]
  directory = /w
EOF
    docker run -it --rm -v $PWD:/w -w /w -v /tmp/gitconfig:/root/.gitconfig $1
}

function run-builder-amd64 () {
    cat <<EOF > /tmp/gitconfig
[safe]
  directory = /w
EOF
    docker run -it --rm --platform linux/amd64 -v $PWD:/w -w /w -v /tmp/gitconfig:/root/.gitconfig ghcr.io/emqx/emqx-builder/5.1-3:1.14.5-25.3.2-1-${1:-ubuntu22.04}
}

function emqx-token() {
    curl --silent -X 'POST' "http://${1:-127.0.0.1}:18083/api/v5/login" -H 'accept: application/json' -H 'Content-Type: application/json' -d '{"username": "admin","password": "public"}' | jq -r ".token"
}

function emqx-curl() {
    curl -s -H "Authorization: Bearer $TOKEN" -X GET "http://${2:-127.0.0.1}:18083/api/v5/$1"
}

function aws-unset() {
    unset AWS_PROFILE
    unset AWS_ACCESS_KEY_ID
    unset AWS_SECRET_ACCESS_KEY
    unset AWS_SESSION_TOKEN
}

function jwtd() {
    jq -R 'split(".") | .[0:2] | map(@base64d) | map(fromjson)'
}

function pr-link() {
    prs=$(gh pr list --state open --author "${1:-@me}" --json number,title,url,createdAt --jq '.[] | "#\(.number) \(.title) (\(.createdAt | fromdateiso8601 | strftime("%Y-%m-%d")))"')
    id=$(echo "$prs" | fzf --prompt="Select PR: " --height 15 --border --ansi | cut -d' ' -f1 | tr -d '#')
    if [[ -n $id ]]; then
        url=$(gh pr view $id --json url | jq -r '.url')
        title=$(gh pr view $id --json title | jq -r '.title')
        echo "<ul><li><a href=\"$url\">$title</a> #$id</li></ul>" | pbcopy-html
    fi
}

# if type rbenv &>/dev/null; then eval "$(rbenv init - zsh)"; fi
# if type direnv &>/dev/null; then eval "$(direnv hook zsh)"; fi
[ -f /opt/gcloud/google-cloud-sdk/path.zsh.inc ] && source /opt/gcloud/google-cloud-sdk/path.zsh.inc
[ -f /opt/gcloud/google-cloud-sdk/completion.zsh.inc ] && source /opt/gcloud/google-cloud-sdk/completion.zsh.inc
[ -f ~/.openai ] && source ~/.openai
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -d $HOMEBREW_PREFIX/opt/util-linux/bin ] && path=($HOMEBREW_PREFIX/opt/util-linux/bin $path)
[ -d ~/.cargo/bin ] && path=(~/.cargo/bin $path)
[ -d ~/.local/bin ] && path=(~/.local/bin $path)
[ -d ~/.asdf ] && path=(~/.asdf/shims $path)

export PATH
