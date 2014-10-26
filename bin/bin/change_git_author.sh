#!/bin/sh

if [ $# -ne 3 ]; then
    echo "Usage: change_git_author.sh my.old@email \"New Name\" my.new@email"
    exit 0
fi

echo "Rewriting $1 to $2 <$3>"

cmd="
an=\"\$GIT_AUTHOR_NAME\"
am=\"\$GIT_AUTHOR_EMAIL\"
cn=\"\$GIT_COMMITTER_NAME\"
cm=\"\$GIT_COMMITTER_EMAIL\"

if [ \"\$GIT_COMMITTER_EMAIL\" = \"${1}\" ]
then
    cn=\"${2}\"
    cm=\"${3}\"
fi
if [ \"\$GIT_AUTHOR_EMAIL\" = \"${1}\" ]
then
    an=\"${2}\"
    am=\"${3}\"
fi

export GIT_AUTHOR_NAME=\"\$an\"
export GIT_AUTHOR_EMAIL=\"\$am\"
export GIT_COMMITTER_NAME=\"\$cn\"
export GIT_COMMITTER_EMAIL=\"\$cm\"
"

git filter-branch --env-filter "${cmd}"
