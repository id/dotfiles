#!/bin/sh

hosts=( )
osx_hosts=( )
ssh="ssh -o ConnectTimeout=1 -o ConnectionAttempts=1"

function maybe_show_err {
    if [ $? -ne 0 ]
    then
        ERROR=$(</tmp/err)
        tmux display-message "$? $ERROR"
    fi
}

cat >/tmp/clip
for h in ${hosts[@]}; do
    cat /tmp/clip | ${ssh} ${h} "cat >/tmp/clip; tmux load-buffer /tmp/clip" 2> /tmp/err
    maybe_show_err
done

for h in ${osx_hosts[@]}; do
    cat /tmp/clip | ${ssh} ${h} "cat | pbcopy" 2> /tmp/err
    maybe_show_err
done
