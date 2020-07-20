#!/usr/bin/env fish

function e -d "Invoke $EDITOR on a given file"
    $EDITOR $argv
end
