#!/usr/bin/env fish

function vsudo -d "Vagrant SSH w/ sudo into given host" -w 'vagrant ssh'
    vagrant ssh -c 'sudo -s' $argv
end
