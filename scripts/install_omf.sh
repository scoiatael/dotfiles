#!/bin/sh
TMP=$(mktemp)
SCRIPT_PATH="$TMP"
curl -L https://github.com/oh-my-fish/oh-my-fish/raw/master/bin/install > $SCRIPT_PATH
less $SCRIPT_PATH
echo "Run 'fish $SCRIPT_PATH' to install omf"
