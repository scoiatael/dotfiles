#!/usr/bin/env sh

git config set --append remote.origin.fetch '+refs/pull/*:refs/remotes/origin/pull/*'
