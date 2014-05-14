#!/bin/sh

clip=$(cat)
err=

hosts=( )
osx_hosts=( )
ssh="ssh -o ConnectTimeout=1 -o ConnectionAttempts=1"

for h in ${hosts[@]}; do
    err=$( ${ssh} ${h} "cat | tmux load-buffer -" <<<"${clip}" 2>&1 )
    [ $? -ne 0 ] && tmux display-message "${err}"
done

for h in ${osx_hosts[@]}; do
    err=$( ${ssh} ${h} "cat | pbcopy" <<<"${clip}" 2>&1 )
    [ $? -ne 0 ] && tmux display-message "${err}"
done
