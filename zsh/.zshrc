fpath=( /usr/share/zsh/site-functions /usr/share/zsh/*/functions ~/.zfunc $fpath)

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

autoload -Uz compinit && compinit

zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
[ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ] && zstyle ':completion:*:*:git:*' script /opt/homebrew/etc/bash_completion.d/git-completion.bash
zstyle :compinstall filename '.zshrc'

unalias run-help 2>/dev/null
alias help=run-help
autoload -Uz run-help
autoload -Uz run-help-git

autoload -U select-word-style && select-word-style bash
autoload -U zmv
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

compdef -d ansible-vault

setopt PROMPT_SUBST ;
if [ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=yes # unstaged (*) and staged (+) changes will be shown next to the branch name
    export GIT_PS1_SHOWSTASHSTATE=yes # '$' will be shown next to the branch name
    export GIT_PS1_SHOWUNTRACKEDFILES=yes # '%' will be shown next to the branch name
    export GIT_PS1_SHOWUPSTREAM=(git verbose) # show number of commits ahead/behind (+/-) upstream and upstream abbrev name
    export GIT_PS1_SHOWCONFLICTSTATE=yes # The prompt will include "|CONFLICT"
    export GIT_PS1_SHOWCOLORHINTS=yes
    export GIT_PS1_DESCRIBE_STYLE='describe'
    source /opt/homebrew/etc/bash_completion.d/git-prompt.sh
    PS1=$'\e[32m%n@%m:\e[1m\e[33m%3~\e[0m\e[1m$(__git_ps1 " (%s)")\e[0m\n'
else
    PS1=$'\e[32m%n@%m:\e[1m\e[33m%2~\e[0m\n'
fi
local rprompt_prefix='%{'$'\e[1A''%}' # one line up
local rprompt_suffix='%{'$'\e[1B''%}' # one line down
RPS1="$rprompt_prefix%F{242}%D %*%f$rprompt_suffix"

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

alias emacs='~/code/emacs/src/emacs'

setopt AUTO_CD

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
[ -f ~/.cargo/env ] && source ~/.cargo/env
[ -d /opt/homebrew/opt/util-linux/bin ] && path+=('/opt/homebrew/opt/util-linux/bin')
[ -d "$HOME/.cargo/bin" ] && path+=("$HOME/.cargo/bin")
[ -d "$HOME/.local/bin" ] && path+=("$HOME/.local/bin")

[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH
