#!/usr/bin/env fish

function vprov -d "vagrant provision"
    vagrant rsync $argv && vagrant provision $argv
end
