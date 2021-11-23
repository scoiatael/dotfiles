#!/usr/bin/env bash

exec curl $CURL_OPTS -w "@$HOME/dotfiles/bash/curl-format.txt" -o /dev/null -s "$1"
