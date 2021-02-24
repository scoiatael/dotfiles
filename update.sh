#!/usr/bin/env bash
set -exuo pipefail

find . -name .stowrc -exec bash -c 'cd $(dirname {}) && stow --dotfiles --adopt .' \;
