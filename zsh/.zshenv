export COLORTERM=truecolor
export HISTSIZE=1000000000
export SAVEHIST=1000000000
export EMACS_SOCKET_NAME="${TMPDIR}/emacs$(id -u)/server"
export EDITOR="${EDITOR} --socket-name ${EMACS_SOCKET_NAME}"
export PROMPT_EOL_MARK=''
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
