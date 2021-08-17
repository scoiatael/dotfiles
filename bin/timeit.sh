#!/usr/bin/env bash

exec curl -w "@$HOME/dotfiles/bash/curl-format.txt" -o /dev/null -s "$1"
