#!/usr/bin/env sh
#
# 1. Commit, if necessary
# 2. Pull
# 3. Push

set -e

cd ~/proj/notes

if [ -n "$(git diff --exit-code)" ]; then
    echo "There are uncommitted changes. Please specify a commit message."

    git add -A
    read -p "(f)lashcards, (j)ournal, or (c)ustom: " choice
    case $choice in
        f) git commit -m "Flashcards";;
        j) git commit -m "Journal";;
        c) git commit;;
        *) git commit;;
    esac
fi

git pull && git push
