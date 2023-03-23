#!/usr/bin/env bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title lpass
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🔐
# @raycast.argument1 { "type": "text", "placeholder": "Query" }
# @raycast.description Search all items in a Lastpass vault, and copy the password of the first search result to the clipboard.

notFound() {
  echo "The query '${BASH_ARGV[0]}' did not return a password."
  exit 1
}

PATH="~/.nix-profile/bin/:$PATH"
if ! command -v lpass &> /dev/null; then
  echo "The Bitwarden CLI is not installed."
  exit 1
fi

# token=$(security find-generic-password -a ${USER} -s raycast-bitwarden -w 2> /dev/null)
# token_status=$?

# session=""
# if [ $token_status -eq 0 ]; then
#   session="--session $token"
# fi

# bw unlock --check $session > /dev/null 2>&1
# unlocked_status=$?

# if [ $unlocked_status -ne 0 ]; then
#   echo "Vault is locked!"
#   exit 1
# fi

item=$(lpass ls | grep "$1")
name=$(echo $item | awk '{ print $1  }') || notFound
password=$(echo $item cut -d '[' -f 2| cut -d ']' -f 1 | awk '{ print $2  }' | xargs lpass show | awk '/password:/ { print $2  }') || notFound

echo -n $password | pbcopy
unset password
echo "Copied the password for '$name' to the clipboard."
exit 0
