#!/usr/bin/env sh
# Commit, if necessary, and push notes.

set -e

cd ~/proj/notes

if [ -n "$(git diff --exit-code)" ]; then
    echo "Cannot push with uncommitted changes. Please specify a commit message."

    read -p "(f)lashcards, (j)ournal, or (c)ustom: " choice
    case $choice in
        f) git commit -m "Flashcards";;
        j) git commit -m "Journal";;
        c) git commit;;
        *) git commit;;
    esac
fi

git push
