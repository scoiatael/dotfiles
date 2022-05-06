#!/usr/bin/env bash

# https://stackoverflow.com/a/53370600/18355339
repo_path=$(git rev-parse --git-dir)

if [ $? -ne 0 ]; then
    exit $?
fi

if [ -d "${repo_path}/rebase-merge" ]; then
    git rebase --continue
elif [ -d "${repo_path}/rebase-apply" ]; then
    git rebase --continue
elif [ -f "${repo_path}/MERGE_HEAD" ]; then
    git merge --continue
elif [ -f "${repo_path}/CHERRY_PICK_HEAD" ]; then
    git cherry-pick --continue
elif [ -f "${repo_path}/REVERT_HEAD" ]; then
    git revert --continue
else
    echo "Nothing in progress?"
    exit 127
fi
