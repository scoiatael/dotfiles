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

notLoggedIn() {
  echo "Please login via 'lpass login  <username>' first"
  exit 1
}

PATH="~/.nix-profile/bin/:$PATH"
if ! command -v lpass &> /dev/null; then
  echo "The LastPass CLI is not installed."
  exit 1
fi

lpass status > /dev/null 2>&1 || notLoggedIn()

item=$(lpass ls | grep "$1")
name=$(echo $item | awk '{ print $1  }') || notFound
id=$(echo $item | cut -d '[' -f 2| cut -d ']' -f 1 | awk '{ print $2  }') || notFound
password=$(lpass show "$id" | awk '/password:/ { print $2  }') || notFound

echo -n $password | pbcopy
unset password
echo "Copied the password for '$name' [ID: $id] to the clipboard."
exit 0
