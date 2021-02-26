#!/usr/bin/env bash
set -exuo pipefail

fd -H --maxdepth 2 '.stowrc' -x bash -c "cd {//} && stow --dotfiles --adopt ." \; .
